module Dataflow.Core where

import Control.Monad (bind, pure, (>>=))
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.RWS (RWST, ask, evalRWST, get, local, put, tell)
import Data.Array (head, tail, zip)
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Function (const, (#), ($))
import Data.Functor ((<#>))
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over)
import Data.Semigroup ((<>))
import Data.Set as Set
import Data.Show (show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Dataflow.Error (TypeError(..))
import Dataflow.Type (Type(..), TVar(..), Scheme(..), typeBool, typeInt)
import Prelude (Unit, discard, (+))
import Dataflow.Expression (Expression(..), Literal(..))
import Dataflow.Substitution (Substitution, compose, apply, class Substituable, ftv)
import Dataflow.TypeEnv (TypeEnv(..), extend)

type InferState
  = { count :: Int
    }

type Constraint
  = Tuple Type Type

type Unifier
  = Tuple Substitution (Array Constraint)

type Infer a
  = RWST TypeEnv (Array Constraint) InferState (Except TypeError) a

-- | Constraint solver monad
type Solve a
  = Either TypeError a

fresh :: Infer Type
fresh = do
  inferState <- get
  put $ inferState { count = inferState.count + 1 }
  let
    name = "t" <> show inferState.count
  name # TV # TVarariable # pure

isRecursive :: forall a. Substituable a => TVar -> a -> Boolean
isRecursive subst t = subst `Set.member` ftv t

nullSubst :: Substitution
nullSubst = Map.empty

bindVariable :: TVar -> Type -> Solve Substitution
bindVariable a t
  | t == TVarariable a = nullSubst # pure
  | isRecursive a t = throwError $ RecursiveType a t
  | otherwise = pure $ Map.singleton a t

createConstraint :: Type -> Type -> Infer Unit
createConstraint t1 t2 = tell [ constraint ]
  where
  constraint = Tuple t1 t2

createClosure :: forall a. TVar -> Scheme -> Infer a -> Infer a
createClosure name scheme m = do
  let
    scope env = cleaned `extend` (Tuple name scheme)
      where
      cleaned = over TypeEnv (Map.delete name) env
  local scope m

instantiate :: Scheme -> Infer Type
instantiate (Forall q t) = do
  q' <- traverse (const fresh) q
  let
    scheme = Map.fromFoldable $ zip q q'
  t # apply scheme # pure

generalize :: Type -> Infer Scheme
generalize t = do
  env <- ask
  let
    qunatifiers = ftv t `Set.difference` ftv env # Set.toUnfoldable
  pure $ Forall qunatifiers t

lookupEnv :: TVar -> Infer Type
lookupEnv var = do
  (TypeEnv env) <- ask
  case Map.lookup var env of
    Nothing -> throwError $ UnboundVariable var
    Just s -> instantiate s

infer :: Expression -> Infer Type
infer = case _ of
  Variable name -> name # lookupEnv
  Lambda param body -> do
    tv <- fresh
    t <- createClosure param (Forall [] tv) $ infer body
    pure $ tv `TArrow` t
  FunctionCall func input -> do
    funcType <- infer func
    inputType <- infer input
    tv <- fresh
    createConstraint funcType (inputType `TArrow` tv)
    pure tv
  Let name value body -> do
    t <- infer value
    inner <- generalize t
    createClosure name inner (infer body)
  If condition onTrue onFalse -> do
    conditionType <- infer condition
    trueType <- infer onTrue
    falseType <- infer onFalse
    createConstraint conditionType typeBool
    createConstraint trueType falseType
    pure trueType
  FixPoint expression -> do
    t <- infer expression
    tv <- fresh
    createConstraint (tv `TArrow` tv) t
    pure tv
  Literal (LInt _) -> pure typeInt
  Literal (LBool _) -> pure typeBool

emptyUnifier :: Unifier
emptyUnifier = Tuple nullSubst []

unifyMany :: Array Type -> Array Type -> Solve Substitution
unifyMany [] [] = pure nullSubst

unifyMany types types' = fromMaybe error subst
  where
  error = throwError $ DifferentLength types types'

  subst = do
    t1s <- tail types
    t2s <- tail types'
    t1 <- head types
    t2 <- head types'
    pure
      $ do
          substitution <- unifies t1 t2
          substitution' <- unifyMany (apply substitution $ t1s) (apply substitution $ t2s)
          pure (substitution' `compose` substitution)

unifies :: Type -> Type -> Solve Substitution
unifies t1 t2
  | t1 == t2 = pure nullSubst

unifies (TVarariable v) t = v `bindVariable` t

unifies t (TVarariable v) = v `bindVariable` t

unifies (TArrow f t) (TArrow f' t') = unifyMany [ f, t ] [ f', t' ]

unifies t1 t2 = throwError $ TypeMissmatch t1 t2

solver :: Unifier -> Solve Substitution
solver (Tuple oldSubst constraints) = fromMaybe (pure oldSubst) newSubst
  where
  newSubst = do
    (Tuple t1 t2) <- head constraints
    c' <- tail constraints
    pure do
      subst1 <- unifies t1 t2
      Tuple (subst1 `compose` oldSubst) (apply subst1 c') # solver

initInfer :: InferState
initInfer = { count: 0 }

-- | Run the constraint solver
runSolve :: (Array Constraint) -> Either TypeError Substitution
runSolve cs = (Tuple nullSubst cs) # solver

runInfer :: forall a. TypeEnv -> Infer a -> Either TypeError (Tuple a (Array Constraint))
runInfer env m = result
  where
  (Identity result) = evalRWST m env initInfer # runExceptT

solveExpression :: TypeEnv -> Expression -> Either TypeError Type
solveExpression env expr = result
  where
  inferResult = infer expr # runInfer env

  solveResult = inferResult <#> snd >>= runSolve

  result = do
    t <- inferResult <#> fst
    subst <- solveResult
    pure $ apply subst t

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty
