module Lunarbox.Data.Dataflow.Class.Substituable where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))

newtype Substitution
  = Substitution (Map.Map TVarName Type)

instance semigroupSubstitution :: Semigroup Substitution where
  append s1 s2 = ((apply s1) <$> s2) `Map.union` s1

derive newtype instance monoidSubstitution :: Monoid Substitution

class Substituable a where
  apply :: Substitution -> a -> a
  ftv :: a -> Set.Set TVar

instance typeSubst :: Substituable Type where
  apply _ t@(TConstant _) = t
  apply s t@(TVarariable a) = (Map.lookup a s) # fromMaybe t
  apply s (TArrow t1 t2) = apply s t1 `TArrow` apply s t2
  ftv (TConstant _) = Set.empty
  ftv (TVarariable a) = Set.singleton a
  ftv (TArrow t1 t2) = ftv t1 `Set.union` ftv t2

instance schemeSubst :: Substituable Scheme where
  apply scheme (Forall quantifiers t) = Forall quantifiers $ apply newScheme t
    where
    newScheme = foldr Map.delete scheme quantifiers
  ftv (Forall as t) = ftv t `Set.difference` (Set.fromFoldable as)

instance arrSubst :: (Substituable a, Foldable f, Functor f) => Substituable (f a) where
  apply = map <<< apply
  ftv = foldr (Set.union <<< ftv) Set.empty

instance envSusbt :: Substituable TypeEnv where
  apply s (TypeEnv env) = env <#> (apply s) # TypeEnv
  ftv (TypeEnv env) = ftv $ Map.values env
