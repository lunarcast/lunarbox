module Lunarbox.Control.Monad.Dataflow.Infer.InferExpression
  ( infer, instantiate
  ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets, modify_)
import Data.Array (find, zip)
import Data.Either (Either(..))
import Data.Lens (over, view)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Traversable (sequence)
import Lunarbox.Control.Monad.Dataflow.Infer (Infer, _constraints, _count, _location, _typeEnv, _usedNames, createConstraint, rememberType, withLocation)
import Lunarbox.Control.Monad.Dataflow.Solve (SolveContext(..), runSolve)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveConstraintSet (solve)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply, ftv)
import Lunarbox.Data.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..), VarName, getLocation)
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNumber)
import Lunarbox.Data.Dataflow.TypeEnv (TypeEnv(..))
import Lunarbox.Data.Dataflow.TypeEnv as TypeEnv
import Lunarbox.Data.Dataflow.TypeError (TypeError(..))

-- Create a fewsh type variable
-- Uses the state from within the Infer monad to prevent duplicates
fresh :: forall l. Ord l => Show l => Infer l Type
fresh = do
  location <- asks $ view _location
  used <- gets $ view _usedNames
  if isJust $ find (_ == show location) used then do
    count <- gets $ view _count
    modify_
      $ over _count (_ + 1)
    pure
      $ TVarariable
      $ TVarName
      $ "t"
      <> show count
  else do
    modify_
      $ over _usedNames (show location : _)
    pure
      $ TVarariable
      $ TVarName
      $ show location

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
instantiate :: forall l. Ord l => Show l => Scheme -> Infer l Type
instantiate (Forall q t) = do
  q' <- sequence $ fresh <$ q
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
lookupEnv :: forall l. Ord l => Show l => VarName -> Infer l Type
lookupEnv var = do
  location <- asks $ view _location
  (TypeEnv env) <- asks $ view _typeEnv
  case Map.lookup var env of
    Nothing -> throwError $ UnboundVariable var location
    Just s -> instantiate s

-- Infers a type and marks it location on the typeMap
infer :: forall l. Ord l => Show l => Expression l -> Infer l Type
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
      Let _ shouldGeneralize name value body -> do
        t <- infer value
        constraints <- gets $ view _constraints
        location <- asks $ view _location
        environment <- asks $ view _typeEnv
        case runSolve (SolveContext { location }) $ solve constraints of
          Left err -> throwError err
          Right substitution ->
            local (over _typeEnv $ apply substitution) do
              generalized <- generalize $ apply substitution t
              createClosure name generalized $ infer body
      FixPoint _ body -> do
        t <- infer body
        tv <- fresh
        createConstraint (tv `TArrow` tv) t
        pure tv
      Chain _ Nil -> fresh
      Chain _ (expression' : Nil) -> infer expression'
      Chain location (expression' : expressions) -> do
        void $ infer expression'
        infer $ Chain location expressions
      Literal _ LNull -> fresh
      Literal _ (LInt _) -> pure typeNumber
      Literal _ (LBool _) -> pure typeBool
      Native _ (NativeExpression scheme _) -> instantiate scheme
    rememberType type'
