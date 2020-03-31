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
import Data.Tuple (Tuple(..))
import Lunarbox.Control.Monad.Dataflow.Infer (Infer, _count, _typeEnv)
import Lunarbox.Data.Dataflow.Constraint (Constraint(..), ConstraintSet(..))
import Lunarbox.Data.Dataflow.TypeError (TypeError(..))
import Lunarbox.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..))
import Lunarbox.Dataflow.Substitution (apply, ftv)
import Lunarbox.Dataflow.Type (Scheme(..), TVar(..), Type(..), typeBool, typeNumber)
import Lunarbox.Dataflow.TypeEnv (TypeEnv(..), extend)

-- Create a fewsh type variable
-- Uses the state from within the Infer monad to prevent duplicates
fresh :: forall l. Infer l Type
fresh = do
  inferState <- gets $ view _count
  modify_ $ over _count (_ + 1)
  pure $ TVarariable $ TV $ "t" <> show inferState

-- Takes 2 types and creates a constraint which says those 2 types should be equal
createConstraint :: forall l. l -> Type -> Type -> Infer l Unit
createConstraint l t1 t2 = tell $ ConstraintSet [ Constraint { leftType: t1, rightType: t2, source: l } ]

-- Create a scope for a variable to be in
createClosure :: forall l a. TVar -> Scheme -> Infer l a -> Infer l a
createClosure name scheme =
  let
    scope (TypeEnv env) = cleaned `extend` (Tuple name scheme)
      where
      cleaned = TypeEnv $ (Map.delete name) env
  in
    local $ over _typeEnv scope

-- The opposite of generalie. Takes a Forall type and creates a type out of it it
instantiate :: forall l. Scheme -> Infer l Type
instantiate (Forall q t) = do
  q' <- traverse (const fresh) q
  let
    scheme = Map.fromFoldable $ zip q q'
  pure $ apply scheme t

-- The opposite of instantiate. Takes a type, finds all the unresolved variables and packs them in a Forall instance. 
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

-- Takes an expression and returns it's type
infer :: forall l. Expression l -> Infer l Type
infer = case _ of
  Variable name location -> lookupEnv location name
  Lambda param body _ -> do
    tv <- fresh
    t <- createClosure param (Forall [] tv) $ infer body
    pure $ tv `TArrow` t
  FunctionCall func input location -> do
    funcType <- infer func
    inputType <- infer input
    tv <- fresh
    createConstraint location funcType (inputType `TArrow` tv)
    pure tv
  Let name value body _ -> do
    t <- infer value
    inner <- generalize t
    createClosure name inner (infer body)
  If condition onTrue onFalse location -> do
    conditionType <- infer condition
    trueType <- infer onTrue
    falseType <- infer onFalse
    createConstraint location conditionType typeBool
    createConstraint location trueType falseType
    pure trueType
  FixPoint expression location -> do
    t <- infer expression
    tv <- fresh
    createConstraint location (tv `TArrow` tv) t
    pure tv
  Native (NativeExpression t _) _ -> pure t
  Literal (LInt _) _ -> pure typeNumber
  Literal (LBool _) _ -> pure typeBool
