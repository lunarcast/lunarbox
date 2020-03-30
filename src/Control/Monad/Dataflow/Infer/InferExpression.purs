module Lunarbox.Control.Monad.Dataflow.Infer.InferExpression where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets, modify_)
import Control.Monad.Writer (tell)
import Data.Array (zip)
import Data.Lens (over, view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lunarbox.Control.Monad.Dataflow.Infer (Infer, _count, _typeEnv)
import Lunarbox.Data.Dataflow.Constraint (ConstraintSet(..))
import Lunarbox.Dataflow.Error (TypeError(..))
import Lunarbox.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..))
import Lunarbox.Dataflow.Substitution (class Substituable, apply, ftv)
import Lunarbox.Dataflow.Type (Scheme(..), TVar(..), Type(..), typeBool, typeNumber)
import Lunarbox.Dataflow.TypeEnv (TypeEnv(..), extend)

fresh :: forall l. Infer l Type
fresh = do
  inferState <- gets $ view _count
  modify_ $ over _count (_ + 1)
  let
    name = "t" <> show inferState
  pure $ TVarariable $ TV $ name

isRecursive :: forall a. Substituable a => TVar -> a -> Boolean
isRecursive subst t = subst `Set.member` ftv t

createConstraint :: forall l. Type -> Type -> Infer l Unit
createConstraint t1 t2 = tell $ ConstraintSet $ [ Tuple t1 t2 ]

createClosure :: forall l a. TVar -> Scheme -> Infer l a -> Infer l a
createClosure name scheme =
  let
    scope (TypeEnv env) = cleaned `extend` (Tuple name scheme)
      where
      cleaned = TypeEnv $ (Map.delete name) env
  in
    local $ over _typeEnv scope

instantiate :: forall l. Scheme -> Infer l Type
instantiate (Forall q t) = do
  q' <- traverse (const fresh) q
  let
    scheme = Map.fromFoldable $ zip q q'
  pure $ apply scheme t

generalize :: forall l. Type -> Infer l Scheme
generalize t = do
  env <- asks $ view _typeEnv
  let
    qunatifiers = ftv t `Set.difference` ftv env # Set.toUnfoldable
  pure $ Forall qunatifiers t

-- Lookup a TypeEnv and return the type. If the type doen't exist an error is thrown 
lookupEnv :: forall l. l -> TVar -> Infer l Type
lookupEnv location var = do
  (TypeEnv env) <- asks $ view _typeEnv
  case Map.lookup var env of
    Nothing -> throwError $ UnboundVariable var location
    Just s -> instantiate s

infer :: forall l. Expression l -> Infer l Type
infer = case _ of
  Variable name location -> lookupEnv location name
  Lambda param body _ -> do
    tv <- fresh
    t <- createClosure param (Forall [] tv) $ infer body
    pure $ tv `TArrow` t
  FunctionCall func input _ -> do
    funcType <- infer func
    inputType <- infer input
    tv <- fresh
    createConstraint funcType (inputType `TArrow` tv)
    pure tv
  Let name value body _ -> do
    t <- infer value
    inner <- generalize t
    createClosure name inner (infer body)
  If condition onTrue onFalse _ -> do
    conditionType <- infer condition
    trueType <- infer onTrue
    falseType <- infer onFalse
    createConstraint conditionType typeBool
    createConstraint trueType falseType
    pure trueType
  FixPoint expression _ -> do
    t <- infer expression
    tv <- fresh
    createConstraint (tv `TArrow` tv) t
    pure tv
  Native (NativeExpression t _) _ -> pure t
  Literal (LInt _) _ -> pure typeNumber
  Literal (LBool _) _ -> pure typeBool
