module Context where

--import PrettyPrint (D (..), Disp (..), Doc, SourcePos, render)
import Unbound.Generics.LocallyNameless as Unbound
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadIO (..),
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

import Ast

---------------------------------

-- Type Checking Monad
-- A stack of four monads carrying various info:
-- IO, Errors, Environment, Freshness
--type TcMonad = Unbound.FreshMT (ReaderT Env (ExceptT Err IO))
--type TcMonad = Unbound.FreshMT (Reader Env) --simple version: fresh and env
type TcMonad = Unbound.FreshMT (ReaderT Env IO)
-- TODO: ^^how simple should this be? do we need IO?

runTcMonad :: TcMonad a -> IO a
runTcMonad m = runReaderT (Unbound.runFreshMT m) emptyEnv

--data Err = Err

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    getCtx :: [Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    getGlobals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    getHints :: [Sig]--,
    -- | what part of the file we are in (for errors/warnings)
    --sourceLocation :: [SourceLocation]
  } deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] 0 []

---------------------------------

lookupType :: TermName -> TcMonad Sig
lookupType v = do
    ctx <- asks getCtx
    return $ lookupVar ctx
        where lookupVar [] = error $ "variable " ++ show v ++ " not found"
              lookupVar (TypeSig sig : ctx)
                  | v == sigName sig = sig
                  | otherwise = lookupVar ctx
              lookupVar (_ : ctx) = lookupVar ctx

-- works like a continuation by executing the computation in a modified env
-- local :: (r -> r) -> m a -> m a
-- pattern match on the Env and modify only the `getCtx` field
extendCtx :: Decl -> TcMonad a -> TcMonad a
extendCtx decl = local (\m@Env{getCtx=ctx} -> m{getCtx=decl:ctx})

--err :: (Disp a) => [a] -> TcMonad b
--err = undefined
--
--warn :: (Disp a) => a -> TcMonad ()
--warn = undefined
--

    {-
x = Unbound.s2n "x"
Env.runTcMonad $ typeCheckTerm (App (Ann (Lam (Unbound.bind x (Var x))) (Pi U (Unbound.bind x U))) U) (Just U)

it works!
        -}
