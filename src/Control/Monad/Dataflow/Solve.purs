module Lunarbox.Control.Monad.Dataflow.Solve
  ( Solve(..)
  , SolveContext(..)
  , SolveState(..)
  , _location
  , runSolve
  , throwTypeError
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Writer (class MonadTell, class MonadWriter, Writer, runWriter, tell)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
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

-- This state keeps track of all the errors
newtype SolveState l
  = SolveState
  { errors :: Array (TypeError l)
  }

derive instance newtypeSolveState :: Newtype (SolveState l) _

derive newtype instance semigroupSolveState :: Semigroup (SolveState l)

derive newtype instance monoiSolveState :: Monoid (SolveState l)

-- Monad used to solve type constraints
newtype Solve l a
  = Solve (ReaderT (SolveContext l) (Writer (SolveState l)) a)

-- Takes a Solve monad and returns Either a TyperError or the inner value 
runSolve :: forall l a. SolveContext l -> Solve l a -> Tuple a (SolveState l)
runSolve ctx (Solve m) = runWriter $ runReaderT m ctx

throwTypeError :: forall l a. Monoid a => (l -> TypeError l) -> Solve l a
throwTypeError getError = mempty <$ m
  where
  m = (asks $ view _location) >>= (tell <<< SolveState <<< { errors: _ } <<< pure <<< getError)

-- Typeclasses
derive instance newtypeSolve :: Newtype (Solve l a) _

derive newtype instance functorSolve :: Functor (Solve l)

derive newtype instance applySolve :: Apply (Solve l)

derive newtype instance applicativeSolve :: Applicative (Solve l)

derive newtype instance bindSolve :: Bind (Solve l)

derive newtype instance monadSolve :: Monad (Solve l)

derive newtype instance monadTellSolve :: MonadTell (SolveState l) (Solve l)

derive newtype instance monadWriterSolve :: MonadWriter (SolveState l) (Solve l)

derive newtype instance monadAskSolve :: MonadAsk (SolveContext l) (Solve l)

derive newtype instance monadReaderSolve :: MonadReader (SolveContext l) (Solve l)
