module Lunarbox.Control.Monad.Dataflow.Solve.Unify (unify, unifyMany) where

import Prelude
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Set as Set
import Lunarbox.Control.Monad.Dataflow.Solve (Solve, throwTypeError)
import Lunarbox.Data.Dataflow.Class.Substituable (class Substituable, Substitution(..), apply, ftv)
import Lunarbox.Data.Dataflow.Type (TVarName, Type(..), typeNull)
import Lunarbox.Data.Dataflow.TypeError (TypeError(..))

-- check if a type is recursive
isRecursive :: forall a. Substituable a => TVarName -> a -> Boolean
isRecursive subst t = subst `Set.member` ftv t

-- Bind a variable to something else in a substitution
bindVariable :: forall l. TVarName -> Type -> Solve l Substitution
bindVariable a t
  | t == TVarariable a = pure mempty
  | isRecursive a t = throwTypeError $ RecursiveType a t
  | otherwise = pure $ Substitution $ Map.singleton a t

unify :: forall l. Type -> Type -> Solve l Substitution
unify t t'
  | t == t' || t == typeNull || t' == typeNull = pure mempty

unify (TVarariable v) t = bindVariable v t

unify t (TVarariable v) = bindVariable v t

unify (TArrow f t) (TArrow f' t') = unifyMany (f : t : Nil) (f' : t' : Nil)

unify t1 t2 = throwTypeError $ TypeMissmatch t1 t2

unifyMany :: forall l. List Type -> List Type -> Solve l Substitution
unifyMany Nil Nil = pure mempty

unifyMany (t : ts) (t' : ts') = do
  substitution <- unify t t'
  substitution' <- unifyMany (apply substitution $ ts) (apply substitution $ ts')
  pure (substitution' <> substitution)

unifyMany types types' = throwTypeError $ DifferentLength types types'
