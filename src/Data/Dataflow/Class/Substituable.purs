module Lunarbox.Data.Dataflow.Class.Substituable where

import Prelude
import Data.Foldable (foldr)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName, Type(..))
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..))

newtype Substitution
  = Substitution (Map.Map TVarName Type)

derive instance eqSubstitution :: Eq Substitution

instance semigroupSubstitution :: Semigroup Substitution where
  append s1@(Substitution m1) (Substitution m2) =
    let
      m12 = (apply s1) <$> m2
    in
      Substitution $ (m12 `Map.union` m1)

derive newtype instance monoidSubstitution :: Monoid Substitution

class Substituable a where
  apply :: Substitution -> a -> a
  ftv :: a -> Set.Set TVarName

instance typeSubst :: Substituable Type where
  apply _ t@(TConstant _) = t
  apply (Substitution s) t@(TVariable _ a) = (Map.lookup a s) # fromMaybe t
  apply s (TArrow t1 t2) = apply s t1 `TArrow` apply s t2
  ftv (TArrow t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TConstant _) = Set.empty
  ftv (TVariable generalize a)
    | generalize = Set.singleton a
    | otherwise = Set.empty

instance schemeSubst :: Substituable Scheme where
  apply (Substitution substitution) (Forall quantifiers t) = Forall quantifiers $ apply newScheme t
    where
    newScheme = Substitution $ foldr Map.delete substitution quantifiers
  ftv (Forall as t) = ftv t `Set.difference` (Set.fromFoldable as)

instance arrSubst :: (Substituable a) => Substituable (List a) where
  apply = map <<< apply
  ftv = foldr (Set.union <<< ftv) Set.empty

instance envSusbt :: Substituable TypeEnv where
  apply s (TypeEnv env) = env <#> (apply s) # TypeEnv
  ftv (TypeEnv env) = ftv $ Map.values env
