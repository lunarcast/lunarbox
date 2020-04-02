module Lunarbox.Control.Monad.Dataflow.Infer.InferExpression
  ( infer
  ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets, modify_)
import Data.Array (zip)
import Data.Lens (over, view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Lunarbox.Control.Monad.Dataflow.Infer (Infer, _count, _location, _typeEnv, createConstraint, rememberType, withLocation)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply, ftv)
import Lunarbox.Data.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..), VarName, getLocation)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNull, typeNumber)
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..))
import Lunarbox.Data.Dataflow.TypeEnv as TypeEnv
import Lunarbox.Data.Dataflow.TypeError (TypeError(..))

-- Create a fewsh type variable
-- Uses the state from within the Infer monad to prevent duplicates
fresh :: forall l. Ord l => Infer l Type
fresh = do
  inferState <- gets $ view _count
  modify_ $ over _count (_ + 1)
  pure $ TVarariable $ TVarName $ "t" <> show inferState

-- Create a scope for a variable to be in
createClosure :: forall l a. Ord l => VarName -> Scheme -> Infer l a -> Infer l a
createClosure name scheme =
  let
    scope (TypeEnv env) = cleaned <> TypeEnv.singleton name scheme
      where
      cleaned = TypeEnv $ (Map.delete name) env
  in
    local $ over _typeEnv scope

-- The opposite of generalie. Takes a Forall type and creates a type out of it it
instantiate :: forall l. Ord l => Scheme -> Infer l Type
instantiate (Forall q t) = do
  q' <- traverse (const fresh) q
  let
    scheme = Substitution $ Map.fromFoldable $ zip q q'
  pure $ apply scheme t

-- The opposite of instantiate. Takes a type, finds all the unresolved variables and packs them in a Forall instance. 
generalize :: forall l. Ord l => Type -> Infer l Scheme
generalize t = do
  env <- asks $ view _typeEnv
  let
    qunatifiers = ftv t `Set.difference` ftv env # Set.toUnfoldable
  pure $ Forall qunatifiers t

-- Lookup a TypeEnv and return the type. If the type doen't exist an error is thrown 
lookupEnv :: forall l. Ord l => VarName -> Infer l Type
lookupEnv var = do
  location <- asks $ view _location
  (TypeEnv env) <- asks $ view _typeEnv
  case Map.lookup var env of
    Nothing -> throwError $ UnboundVariable var location
    Just s -> instantiate s

-- Infers a type and marks it location on the typeMap
infer :: forall l. Ord l => Expression l -> Infer l Type
infer expression =
  withLocation (getLocation expression) do
    type' <- case expression of
      Variable _ name -> do
        lookupEnv name
      Lambda _ param body -> do
        tv <- fresh
        t <- createClosure param (Forall [] tv) $ infer body
        pure $ tv `TArrow` t
      FunctionCall _ func input -> do
        funcType <- infer func
        inputType <- infer input
        tv <- fresh
        createConstraint funcType (inputType `TArrow` tv)
        pure tv
      Let _ name value body -> do
        t <- infer value
        inner <- generalize t
        createClosure name inner (infer body)
      If _ condition onTrue onFalse -> do
        conditionType <- infer condition
        trueType <- infer onTrue
        falseType <- infer onFalse
        createConstraint conditionType typeBool
        createConstraint trueType falseType
        pure trueType
      FixPoint _ body -> do
        t <- infer body
        tv <- fresh
        createConstraint (tv `TArrow` tv) t
        pure tv
      Native _ (NativeExpression t _) -> pure t
      Literal _ (LInt _) -> pure typeNumber
      Literal _ (LBool _) -> pure typeBool
      Literal _ LNull -> pure typeNull
    rememberType type'