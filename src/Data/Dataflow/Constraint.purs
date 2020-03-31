module Lunarbox.Data.Dataflow.Constraint
  ( Constraint(..)
  , ConstraintSet(..)
  , _leftType
  , _rightType
  , _source
  ) where

import Prelude
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Type (Type)

newtype Constraint l
  = Constraint
  { leftType :: Type
  , rightType :: Type
  , source :: l
  }

derive instance newtypeConstraint :: Newtype (Constraint l) _

_leftType :: forall l. Lens' (Constraint l) Type
_leftType = iso unwrap wrap <<< prop (SProxy :: _ "leftType")

_rightType :: forall l. Lens' (Constraint l) Type
_rightType = iso unwrap wrap <<< prop (SProxy :: _ "rightType")

_source :: forall l. Lens' (Constraint l) l
_source = iso unwrap wrap <<< prop (SProxy :: _ "source")

newtype ConstraintSet l
  = ConstraintSet (Array (Constraint l))

derive newtype instance semigroupConstraintSet :: Semigroup (ConstraintSet l)

derive newtype instance monoidConstraintSet :: Monoid (ConstraintSet l)
