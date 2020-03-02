module Dataflow.Core where

import Control.Monad (class Functor, bind, pure, (>>=))
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.RWS (RWST, ask, evalRWST, get, local, put, tell)
import Data.Array (fold, head, tail, zip)
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Eq (class Eq, (==))
import Data.Foldable (class Foldable, foldr)
import Data.Function (const, (#), ($), (<<<))
import Data.Functor ((<$>), (<#>), map)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Set as Set
import Data.Show (class Show, show)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (Unit, discard, (+))

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

derive instance literalOrd :: Ord Literal

data Expression
  = Variable TVar
  | FunctionCall Expression Expression
  | Lambda TVar Expression
  | Literal Literal
  | Let TVar Expression Expression
  | If Expression Expression Expression
  | FixPoint Expression

derive instance expressinEq :: Eq Expression

type Declaration
  = Tuple String Expression

data Program
  = Program (Array Declaration) Expression

derive instance programEq :: Eq Program

newtype TVar
  = TV String

derive instance tvarEq :: Eq TVar

derive instance tvarOrd :: Ord TVar

instance tvarShow :: Show TVar where
  show (TV name) = name

data Type
  = TConstant String
  | TArrow Type Type
  | TVarariable TVar

derive instance typeEq :: Eq Type

derive instance typeOrd :: Ord Type

isArrow :: Type -> Boolean
isArrow = case _ of
  TArrow _ _ -> true
  _ -> false

printType :: Boolean -> Type -> String
printType _ (TVarariable v) = show v

printType _ (TConstant s) = s

printType p (TArrow from to) = if p then "(" <> result <> ")" else result
  where
  prefix = printType (isArrow from) from

  result = prefix <> " -> " <> show to

instance typeShow :: Show Type where
  show = printType false

typeInt :: Type
typeInt = TConstant "Int"

typeBool :: Type
typeBool = TConstant "Bool"

data Scheme
  = Forall (Array TVar) Type

instance showScheme :: Show Scheme where
  show (Forall [] t) = show t
  show (Forall quantifiers t) = "forall" <> fold (quantifiers <#> (\(TV n) -> " " <> n)) <> ". " <> show t

newtype TypeEnv
  = TypeEnv (Map.Map TVar Scheme)

derive instance teNewType :: Newtype TypeEnv _

extend :: TypeEnv -> Tuple TVar Scheme -> TypeEnv
extend (TypeEnv env) (Tuple x s) = TypeEnv $ Map.insert x s env

type TypeError
  = String

type InferState
  = { count :: Int
    }

type Constraint
  = Tuple Type Type

type Unifier
  = Tuple Substitution (Array Constraint)

type Infer a
  = RWST TypeEnv (Array Constraint) InferState (Except String) a

-- | Constraint solver monad
type Solve a
  = Either TypeError a

type Substitution
  = Map.Map TVar Type

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = ((apply s1) <$> s2) `Map.union` s1

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
  | isRecursive a t = throwError $ "Recursive type: " <> show a <> show t
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
    Nothing -> throwError $ "Unbound variable" <> show var
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
  error = throwError $ "Cannot unify type " <> show types <> " with type " <> show types'

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

unifies t1 t2 = throwError $ "Cannot unify type " <> show t1 <> " with type " <> show t2

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
