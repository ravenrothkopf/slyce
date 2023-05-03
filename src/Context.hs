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

import Ast ( TermName, Sig, Decl )

---------------------------------

-- Type Checking Monad
-- A stack of four monads carrying various info:
-- IO, Errors, Environment, Freshness
--type TcMonad = Unbound.FreshMT (ReaderT Env (ExceptT Err IO))
--type TcMonad = Unbound.FreshMT (Reader Env) --simple version: fresh and env
type TcMonad = Unbound.FreshMT (ReaderT Env IO)
-- TODO: ^^how simple should this be? do we need IO?

-- TODO: what is the type?
runTcMonad :: TcMonad a -> IO a
runTcMonad m = runReaderT (Unbound.runFreshMT m) emptyEnv

--data Err = Err

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    ctx :: [Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [Sig]--,
    -- | what part of the file we are in (for errors/warnings)
    --sourceLocation :: [SourceLocation]
  }

emptyEnv :: Env
emptyEnv =
  Env
    { ctx = [],
      globals = 0,
      hints = []--,
      --sourceLocation = []
    }

---------------------------------

lookupType :: TermName -> TcMonad Sig
lookupType = undefined

extendCtx :: Decl -> TcMonad a -> TcMonad a
extendCtx = undefined

--err :: (Disp a) => [a] -> TcMonad b
--err = undefined
--
--warn :: (Disp a) => a -> TcMonad ()
--warn = undefined
