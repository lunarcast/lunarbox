module Lunarbox.Control.Monad.Dataflow.Infer.InferExpression
  ( infer
  ) where

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
import Lunarbox.Control.Monad.Dataflow.Infer (Infer, _count, _location, _typeEnv, withLocation)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply, ftv)
import Lunarbox.Data.Dataflow.Constraint (Constraint(..), ConstraintSet(..))
import Lunarbox.Data.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..), VarName)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNumber)
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..))
import Lunarbox.Data.Dataflow.TypeEnv as TypeEnv
import Lunarbox.Data.Dataflow.TypeError (TypeError(..))

-- Create a fewsh type variable
-- Uses the state from within the Infer monad to prevent duplicates
fresh :: forall l. Infer l Type
fresh = do
  inferState <- gets $ view _count
  modify_ $ over _count (_ + 1)
  pure $ TVarariable $ TVarName $ "t" <> show inferState

-- Takes 2 types and creates a constraint which says those 2 types should be equal
createConstraint :: forall l. Type -> Type -> Infer l Unit
createConstraint t1 t2 = do
  source <- asks $ view _location
  tell $ ConstraintSet [ Constraint { leftType: t1, rightType: t2, source } ]

-- Create a scope for a variable to be in
createClosure :: forall l a. VarName -> Scheme -> Infer l a -> Infer l a
createClosure name scheme =
  let
    scope (TypeEnv env) = cleaned <> TypeEnv.singleton name scheme
      where
      cleaned = TypeEnv $ (Map.delete name) env
  in
    local $ over _typeEnv scope

-- The opposite of generalie. Takes a Forall type and creates a type out of it it
instantiate :: forall l. Scheme -> Infer l Type
instantiate (Forall q t) = do
  q' <- traverse (const fresh) q
  let
    scheme = Substitution $ Map.fromFoldable $ zip q q'
  pure $ apply scheme t

-- The opposite of instantiate. Takes a type, finds all the unresolved variables and packs them in a Forall instance. 
generalize :: forall l. Type -> Infer l Scheme
generalize t = do
  env <- asks $ view _typeEnv
  let
    qunatifiers = ftv t `Set.difference` ftv env # Set.toUnfoldable
  pure $ Forall qunatifiers t

-- Lookup a TypeEnv and return the type. If the type doen't exist an error is thrown 
lookupEnv :: forall l. VarName -> Infer l Type
lookupEnv var = do
  location <- asks $ view _location
  (TypeEnv env) <- asks $ view _typeEnv
  case Map.lookup var env of
    Nothing -> throwError $ UnboundVariable var location
    Just s -> instantiate s

-- Takes an expression and returns it's type
infer :: forall l. Expression l -> Infer l Type
infer = case _ of
  Variable location name ->
    withLocation location do
      lookupEnv name
  Lambda location param body ->
    withLocation location do
      tv <- fresh
      t <- createClosure param (Forall [] tv) $ infer body
      pure $ tv `TArrow` t
  FunctionCall location func input ->
    withLocation location do
      funcType <- infer func
      inputType <- infer input
      tv <- fresh
      createConstraint funcType (inputType `TArrow` tv)
      pure tv
  Let location name value body ->
    withLocation location do
      t <- infer value
      inner <- generalize t
      createClosure name inner (infer body)
  If location condition onTrue onFalse ->
    withLocation location do
      conditionType <- infer condition
      trueType <- infer onTrue
      falseType <- infer onFalse
      createConstraint conditionType typeBool
      createConstraint trueType falseType
      pure trueType
  FixPoint location expression ->
    withLocation location do
      t <- infer expression
      tv <- fresh
      createConstraint (tv `TArrow` tv) t
      pure tv
  -- those 3 branches do not need the location
  Native _ (NativeExpression t _) -> pure t
  Literal _ (LInt _) -> pure typeNumber
  Literal _ (LBool _) -> pure typeBool
