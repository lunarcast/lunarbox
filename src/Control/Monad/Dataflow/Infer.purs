module Lunarbox.Control.Monad.Dataflow.Infer
  ( InferState(..)
  , InferOutput(..)
  , InferEnv(..)
  , Infer(..)
  , _count
  , _usedNames
  , _location
  , _typeEnv
  , _constraints
  , _typeMap
  , runInfer
  , withLocation
  , createConstraint
  , rememberType
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (RWST, asks, evalRWST, local, tell)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (class MonadState)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Either (Either)
import Data.Lens (Lens', iso, set, view)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
import Lunarbox.Data.Dataflow.Constraint (Constraint(..), ConstraintSet(..))
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv)
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Lens (newtypeIso)

-- This is the output accumulated by the infer monad.
-- It contains a set of constraints 
-- and a map of location -> type paris
newtype InferOutput l
  = InferOutput
  { constraints :: ConstraintSet l
  , typeMap :: Map.Map l Type
  }

derive instance newtypeInferOutput :: Newtype (InferOutput l) _

derive newtype instance semigroupInferOutput :: Ord l => Semigroup (InferOutput l)

derive newtype instance monoidInferOutput :: Ord l => Monoid (InferOutput l)

_constraints :: forall l. Lens' (InferOutput l) (ConstraintSet l)
_constraints = newtypeIso <<< prop (SProxy :: _ "constraints")

_typeMap :: forall l. Lens' (InferOutput l) (Map.Map l Type)
_typeMap = newtypeIso <<< prop (SProxy :: _ "typeMap")

newtype InferState
  = InferState
  { count :: Int
  , usedNames :: List String
  }

derive instance newtypeInferState :: Newtype InferState _

instance semigruopInferState :: Semigroup InferState where
  append (InferState { count, usedNames }) (InferState { count: count', usedNames: usedNames' }) =
    InferState
      { count: count + count'
      , usedNames: usedNames <> usedNames'
      }

instance monoidInferState :: Monoid InferState where
  mempty = InferState { count: 0, usedNames: Nil }

_count :: Lens' InferState Int
_count = iso unwrap wrap <<< prop (SProxy :: _ "count")

_usedNames :: Lens' InferState (List String)
_usedNames = iso unwrap wrap <<< prop (SProxy :: _ "usedNames")

newtype InferEnv l
  = InferEnv
  { typeEnv :: TypeEnv
  , location :: l
  }

derive instance newtypeInferEnv :: Newtype (InferEnv l) _

_typeEnv :: forall l. Lens' (InferEnv l) TypeEnv
_typeEnv = iso unwrap wrap <<< prop (SProxy :: _ "typeEnv")

_location :: forall l. Lens' (InferEnv l) l
_location = iso unwrap wrap <<< prop (SProxy :: _ "location")

-- The infer monad is the place where the type inference algorithm runs
newtype Infer l a
  = Infer (RWST (InferEnv l) (InferOutput l) InferState (Except (TypeError l)) a)

-- This is a helper to transform an Infer monad into a single value
runInfer :: forall l a. InferEnv l -> Infer l a -> Either (TypeError l) (Tuple a (InferOutput l))
runInfer env (Infer m) = runExcept $ evalRWST m env mempty

-- run a monad in a specific location
withLocation :: forall a l. Ord l => l -> Infer l a -> Infer l a
withLocation = local <<< set _location

-- Helper to create a constraint at the current location given 2 types
createConstraint :: forall l. Ord l => Type -> Type -> Infer l Unit
createConstraint typeLeft typeRight = do
  source <- asks $ view _location
  tell
    $ InferOutput
        { constraints:
          ConstraintSet $ pure
            $ Constraint
                { source
                , typeRight
                , typeLeft
                }
        , typeMap: mempty
        }

-- helper to mark a type in the typemap at the current location
rememberType :: forall l. Ord l => Type -> Infer l Type
rememberType type' = do
  location <- asks $ view _location
  tell
    $ InferOutput
        { constraints: mempty
        , typeMap: Map.singleton location type'
        }
  pure type'

derive newtype instance functorInfer :: Functor (Infer l)

derive newtype instance applyInfer :: Ord l => Apply (Infer l)

derive newtype instance applicativeInfer :: Ord l => Applicative (Infer l)

derive newtype instance bindInfer :: Ord l => Bind (Infer l)

derive newtype instance monadInfer :: Ord l => Monad (Infer l)

derive newtype instance monadAskInfer :: Ord l => MonadAsk (InferEnv l) (Infer l)

derive newtype instance monadReaderInfer :: Ord l => MonadReader (InferEnv l) (Infer l)

derive newtype instance monadTellInfer :: Ord l => MonadTell (InferOutput l) (Infer l)

derive newtype instance monadWriterInfer :: Ord l => MonadWriter (InferOutput l) (Infer l)

derive newtype instance monadStateInfer :: Ord l => MonadState InferState (Infer l)

derive newtype instance monadThrowInfer :: Ord l => MonadThrow (TypeError l) (Infer l)
