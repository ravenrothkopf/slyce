module Context
    ( runTcMonad
    , TcMonad
    , Env(..)
    , lookupType
    , lookupDef
    , lookupHint
    , extendCtx
    , extendCtxs
    , extendHints
    , extendSourceLocation
    , warn
    , err
    )
    where

--import PrettyPrint (D (..), Disp (..), Doc, SourcePos, render)
import Unbound.Generics.LocallyNameless as Unbound
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

import Ast

---------------------------------

-- Type Checking Monad
-- A stack of four monads carrying various info:
-- IO, Errors, Environment, Freshness
type TcMonad = Unbound.FreshMT (ReaderT Env (ExceptT Err IO))

runTcMonad :: TcMonad a -> IO (Either Err a)
runTcMonad m = runExceptT $ runReaderT (Unbound.runFreshMT m) emptyEnv

data SourceLocation where
    SourceLocation :: SourcePos -> Term -> SourceLocation

data Err = Err [SourceLocation] String

instance Show Err where
    show (Err [] msg) = msg ++ "\nin the expression:\nPosition Unknown"
    show (Err ((SourceLocation p term) : _) msg) =
        show p ++ msg ++ "\nin the expression:\n" ++ show term

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
    getLoc :: [SourceLocation]
  }

emptyEnv :: Env
emptyEnv = Env { getCtx   = []
               , getHints = []
               , getLoc   = []
               }

---------------------------------

lookupType :: TermName -> TcMonad Sig
lookupType v = do
    ctx <- asks getCtx
    case lookupVar ctx of
        Nothing  -> err $ "Variable not found: " ++ show v
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

-- works like a continuation by executing the computation in a modified env
-- local :: (r -> r) -> m a -> m a
-- pattern match on the Env and modify only the `getCtx` field
extendCtx :: Decl -> TcMonad a -> TcMonad a
extendCtx decl = local (\m@Env{getCtx = ctx} -> m{getCtx = decl:ctx})

extendCtxs :: [Decl] -> TcMonad a -> TcMonad a
extendCtxs decls = local (\m@Env{getCtx = ctx} -> m{getCtx = decls++ctx})

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

-- | Throw an error
err :: String -> TcMonad b
err d = do
    loc <- asks getLoc
    throwError $ Err loc d

-- | Print a warning
warn :: (Show a) => a -> TcMonad ()
warn e = do
    loc <- asks getLoc
    liftIO $ putStrLn $ "warning: " ++ (show $ Err loc (show e))
