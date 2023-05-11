module Context
    ( runTcMonad
    , TcMonad
    , Env(..)
    , WhichCon(..)
    , lookupType
    , lookupDef
    , lookupHint
    , lookupWhichCon
    , lookupDConAll
    , lookupDCon
    , lookupTCon
    , extendCtx
    , extendCtxs
    , extendCtxTele
    , extendHints
    , extendErr
    , extendSourceLocation
    , getSourceLocation
    , getContext
    , warn
    , err
    , traceMonad
    )
    where

--import PrettyPrint (D (..), Disp (..), Doc, SourcePos, render)
import Unbound.Generics.LocallyNameless as Unbound
import qualified Data.Set as Set
import Debug.Trace (trace)
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadIO (..),
    catchError,
    runExceptT,
    unless,
    --void,
  )
import Control.Monad.Reader
  ( MonadReader (local),
    ReaderT (runReaderT),
    Reader, runReader,
    asks,
  )
import Control.Applicative ((<|>))
import Data.List (intercalate, find, lookup)
--import Data.Maybe (listToMaybe)

import Ast

import PrettyPrint

---------------------------------

-- Type Checking Monad
-- A stack of four monads carrying various info:
-- IO, Errors, Environment, Freshness
type TcMonad = Unbound.FreshMT (ReaderT Env (ExceptT Err IO))

runTcMonad :: TcMonad a -> IO (Either Err a)
runTcMonad m = runExceptT $ runReaderT (Unbound.runFreshMT m) emptyEnv

data SourceLocation where
    SourceLocation :: SourcePos -> Term -> SourceLocation

instance Show SourceLocation where
    show (SourceLocation p t) = show p ++ show t

data Err = Err [SourceLocation] String

instance Show Err where
    show (Err [] msg) = msg-- ++ "\nin the expression:\nPosition Unknown"
    show (Err ((SourceLocation p term) : _) msg) =
        show p ++ msg ++ "\nin the expression:\n" ++ show term ++ "\n" ++ ppTerm term

instance Semigroup Err where
  (Err src1 d1) <> (Err src2 d2) = Err (src1 ++ src2) (d1 `mappend` d2)

instance Monoid Err where
  mempty = Err [] mempty
  mappend (Err src1 d1) (Err src2 d2) = Err (src1 ++ src2) (d1 `mappend` d2)


-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    getCtx :: [Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    --getGlobals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    getHints :: [Sig],
    -- | what part of the file we are in (for errors/warnings)
    getLoc :: [SourceLocation],
    getCNames :: ConstructorNames
  }

emptyEnv :: Env
emptyEnv = Env { getCtx   = []
               , getHints = []
               , getLoc   = []
               , getCNames = emptyConstructorNames
               }

---------------------------------
-- Lookup functions
---------------------------------

lookupType :: TermName -> TcMonad Sig
lookupType v = do
    ctx <- asks getCtx
    case lookupVar ctx of
        Nothing  -> err $ "Variable not found: " ++ show v ++ "\nin context: " ++ (intercalate "\n" $ map show $ take 5 ctx)
        Just sig -> return sig
        where lookupVar [] = Nothing
              lookupVar (TypeSig sig : ctx)
                  | v == sigName sig = Just sig
                  | otherwise = lookupVar ctx
              lookupVar (_ : ctx) = lookupVar ctx

lookupDef :: TermName -> TcMonad (Maybe Term)
lookupDef x = do
    ctx <- asks getCtx
    return $ foldr (\d acc -> (checkDecl d) <|> acc) Nothing ctx
        where checkDecl (Def y term) | x == y = Just term
              checkDecl _            = Nothing

lookupHint :: TermName -> TcMonad (Maybe Sig)
lookupHint x = do
    hints <- asks getHints
    return $ foldr (\h acc -> (checkHint h) <|> acc) Nothing hints
        where checkHint s@(Sig y t) | x == y = Just s
              checkHint _           = Nothing

-- returns list of TCon names, telescope of type parameters, and constructor
-- def with that DCon name.
lookupDConAll :: DCName -> TcMonad [(TCName, (Telescope, ConstructorDef))]
lookupDConAll dcname = do
    ctx <- asks getCtx
    scanCtx ctx
        where scanCtx [] = return []
              scanCtx ((Data t' tele cs) : ctx) =
                  case find (\(ConstructorDef _ d _) -> d == dcname) cs of
                      Nothing -> scanCtx ctx
                      Just c -> do
                          more <- scanCtx ctx
                          return $ (t', (tele, c)) : more
              scanCtx (_:ctx) = scanCtx ctx

-- | Return the data constructor definition (type param telescope and
-- constructor arg telescope) for a given DCon name and associated TCon name.
-- We must return the type param telescope in order to check if the type is
-- parameterized, since the type of data constructors cannot be inferred if the
-- type is parameterized.
lookupDCon :: DCName -> TCName -> TcMonad (Telescope, Telescope)
lookupDCon dcname tcname = do
    matches <- lookupDConAll dcname
    case lookup tcname matches of
        Just (ttele, ConstructorDef _ _ dtele) -> return (ttele, dtele)
        Nothing -> err $ "Cannot find data constructor " ++ dcname ++
                   " for type " ++ tcname ++ ". Potential matches were:\n" ++
                   show matches

-- Telescope is arguments to the TCon
-- ConstructorDefs are the data constructors
-- Nothing in the case of DataSig
lookupTCon :: TCName -> TcMonad (Telescope, Maybe [ConstructorDef])
lookupTCon tcname = do
    ctx <- asks getCtx
    scanCtx ctx
        where scanCtx [] = err $ "Type constructor not found: " ++ tcname
              scanCtx ((Data t' tele cs) : ctx) =
                  if t' == tcname
                  then return (tele, Just cs)
                  else scanCtx ctx
              scanCtx ((DataSig t' tele) : ctx) =
                  if t' == tcname
                  then return (tele, Nothing)
                  else scanCtx ctx
              scanCtx (_:ctx) = scanCtx ctx

data WhichCon = TCon | DCon
lookupWhichCon :: String -> TcMonad WhichCon
lookupWhichCon name = do
    cnames <- asks getCNames
    let f | name `Set.member` (tconNames cnames) = return TCon
          | name `Set.member` (dconNames cnames) = return DCon
          | otherwise = err $ "Constructor not found: " ++ name
    f

---------------------------------
-- | Extension functions
---------------------------------

-- works like a continuation by executing the computation in a modified env
-- local :: (r -> r) -> m a -> m a
-- pattern match on the Env and modify only the `getCtx` field
extendCtx :: Decl -> TcMonad a -> TcMonad a
extendCtx decl = local (\m@Env{getCtx = ctx} -> m{getCtx = decl:ctx})

extendCtxs :: [Decl] -> TcMonad a -> TcMonad a
extendCtxs decls = local (\m@Env{getCtx = ctx} -> m{getCtx = decls++ctx})

extendCtxTele :: [Decl] -> TcMonad a -> TcMonad a
extendCtxTele [] m = m
extendCtxTele (Def x t2 : tele) m =
  extendCtx (Def x t2) $ extendCtxTele tele m
extendCtxTele (TypeSig sig : tele) m =
  extendCtx (TypeSig sig) $ extendCtxTele tele m
extendCtxTele ( _ : tele) m = err $ "Invalid telescope " ++ show tele

extendHints :: [Sig] -> TcMonad a -> TcMonad a
extendHints h = local (\m@Env{getHints = hints} -> m{getHints = h++hints})

extendErr :: TcMonad a -> String -> TcMonad a
extendErr comp msg' =
  comp `catchError` (\(Err src msg) ->
    throwError $ Err src (msg ++ msg')) -- rethrow error

-- | Push a new source position on the location stack.
extendSourceLocation :: SourcePos -> Term -> TcMonad a -> TcMonad a
extendSourceLocation pos term =
  local (\e@Env{getLoc = locs} -> e{getLoc = SourceLocation pos term : locs})

getSourceLocation :: TcMonad [SourceLocation]
getSourceLocation = asks getLoc

getContext :: TcMonad [Decl]
getContext = asks getCtx

-- | Throw an error
err :: String -> TcMonad b
err d = do
    loc <- asks getLoc
    throwError $ Err loc d

-- | Print a warning
warn :: String -> TcMonad ()
warn e = do
    loc <- asks getLoc
    liftIO $ putStrLn $ "warning: " ++ (show $ Err loc e)

traceMonad :: (Show a, Monad m) => String -> a -> m a
traceMonad s x = trace ("\t" ++ s ++ show x ++ "\n") (return x)
