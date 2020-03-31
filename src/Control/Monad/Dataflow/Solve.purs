module Lunarbox.Control.Monad.Dataflow.Solve where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.TypeError (TypeError)

-- Monad used to solve type constraints
newtype Solve l a
  = Solve (Except (TypeError l) a)

-- Takes a Solve monad and returns Either a TyperError or the inner value 
runSolve :: forall l a. Solve l a -> Either (TypeError l) a
runSolve = runExcept <<< unwrap

-- Typeclasses
derive instance newtypeSolve :: Newtype (Solve l a) _

derive newtype instance functorSolve :: Functor (Solve l)

derive newtype instance applySolve :: Apply (Solve l)

derive newtype instance applicativeSolve :: Applicative (Solve l)

derive newtype instance bindSolve :: Bind (Solve l)

derive newtype instance monadSolve :: Monad (Solve l)

derive newtype instance monadThrowSolve :: MonadThrow (TypeError l) (Solve l)
