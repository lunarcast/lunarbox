module Lunarbox.Control.Monad.Dataflow.Solve
  ( Solve(..)
  , SolveContext(..)
  , _location
  , runSolve
  , throwTypeError
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Data.Either (Either)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Lens (newtypeIso)

-- The solver context is the env all solving occurs in.
-- It stores the location the solving occurs at.
newtype SolveContext l
  = SolveContext
  { location :: l
  }

derive instance newtypeSolveContent :: Newtype (SolveContext l) _

_location :: forall l. Lens' (SolveContext l) l
_location = newtypeIso <<< prop (SProxy :: _ "location")

-- Monad used to solve type constraints
newtype Solve l a
  = Solve (ReaderT (SolveContext l) (Except (TypeError l)) a)

-- Takes a Solve monad and returns Either a TyperError or the inner value 
runSolve :: forall l a. SolveContext l -> Solve l a -> Either (TypeError l) a
runSolve ctx (Solve m) = runExcept $ runReaderT m ctx

throwTypeError :: forall l a. (l -> TypeError l) -> Solve l a
throwTypeError getError = (asks $ view _location) >>= (throwError <<< getError)

-- Typeclasses
derive instance newtypeSolve :: Newtype (Solve l a) _

derive newtype instance functorSolve :: Functor (Solve l)

derive newtype instance applySolve :: Apply (Solve l)

derive newtype instance applicativeSolve :: Applicative (Solve l)

derive newtype instance bindSolve :: Bind (Solve l)

derive newtype instance monadSolve :: Monad (Solve l)

derive newtype instance monadThrowSolve :: MonadThrow (TypeError l) (Solve l)

derive newtype instance monadAskSolve :: MonadAsk (SolveContext l) (Solve l)

derive newtype instance monadReaderSolve :: MonadReader (SolveContext l) (Solve l)
