module Lunarbox.Data.Dataflow.Constraint
  ( Constraint(..)
  , ConstraintSet(..)
  , _typeLeft
  , _typeRight
  , _source
  ) where

import Prelude
import Data.Lens (Lens', iso, over)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Dataflow.Class.Substituable (class Substituable, apply, ftv)
import Lunarbox.Data.Dataflow.Type (Type)

newtype Constraint l
  = Constraint
  { typeLeft :: Type
  , typeRight :: Type
  , source :: l
  }

derive instance eqConstraint :: Eq l => Eq (Constraint l)

derive instance newtypeConstraint :: Newtype (Constraint l) _

instance substiuableConstraint :: Substituable (Constraint l) where
  ftv (Constraint { typeLeft, typeRight }) = ftv typeLeft `Set.union` ftv typeRight
  apply substitution =
    let
      applySubstitution = apply substitution
    in
      over _typeLeft applySubstitution <<< over _typeRight applySubstitution

_typeLeft :: forall l. Lens' (Constraint l) Type
_typeLeft = iso unwrap wrap <<< prop (SProxy :: _ "typeLeft")

_typeRight :: forall l. Lens' (Constraint l) Type
_typeRight = iso unwrap wrap <<< prop (SProxy :: _ "typeRight")

_source :: forall l. Lens' (Constraint l) l
_source = iso unwrap wrap <<< prop (SProxy :: _ "source")

newtype ConstraintSet l
  = ConstraintSet (List (Constraint l))

derive instance eqConstraintSet :: Eq l => Eq (ConstraintSet l)

derive newtype instance semigroupConstraintSet :: Semigroup (ConstraintSet l)

derive newtype instance monoidConstraintSet :: Monoid (ConstraintSet l)
