module Lunarbox.Data.Dataflow.Expression
  ( Literal(..)
  , NativeExpression(..)
  , Expression(..)
  , VarName(..)
  , functionDeclaration
  , getLocation
  , toMap
  , lookup
  ) where

import Prelude
import Data.List (List, foldr)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type (Type)

newtype VarName
  = VarName String

derive instance eqVarName :: Eq VarName

derive instance ordVarName :: Ord VarName

derive newtype instance showVarName :: Show VarName

derive instance newtypeVarName :: Newtype VarName _

data Literal
  = LInt Int
  | LBool Boolean

derive instance literalEq :: Eq Literal

data NativeExpression
  = NativeExpression Type RuntimeValue

derive instance eqNativeExpression :: Eq NativeExpression

data Expression l
  = Variable l VarName
  | FunctionCall l (Expression l) (Expression l)
  | Lambda l VarName (Expression l)
  | Literal l Literal
  | Let l VarName (Expression l) (Expression l)
  | If l (Expression l) (Expression l) (Expression l)
  | FixPoint l (Expression l)
  | Native l NativeExpression

-- Takes a list of argument names and a body and creates the body of a function
functionDeclaration :: forall l. l -> Expression l -> List VarName -> Expression l
functionDeclaration = foldr <<< Lambda

-- Given an Expression extract it's location
getLocation :: forall l. Expression l -> l
getLocation = case _ of
  Variable l _ -> l
  FunctionCall l _ _ -> l
  Lambda l _ _ -> l
  Literal l _ -> l
  Let l _ _ _ -> l
  If l _ _ _ -> l
  FixPoint l _ -> l
  Native l _ -> l

-- Takes an Expression and transforms it into a map of location -> expression pairs
toMap :: forall l. Ord l => Expression l -> Map.Map l (Expression l)
toMap expression =
  Map.singleton (getLocation expression) expression
    <> case expression of
        FunctionCall _ calee input -> toMap calee <> toMap input
        Lambda _ _ body -> toMap body
        Let _ _ value body -> toMap value <> toMap body
        If _ condition then' else' -> toMap condition <> toMap then' <> toMap else'
        FixPoint _ body -> toMap body
        _ -> mempty

-- Tries finding the expression at a certain location
lookup :: forall l. Ord l => l -> Expression l -> Maybe (Expression l)
lookup key = Map.lookup key <<< toMap

-- Typecalss instances
derive instance expressinEq :: Eq l => Eq (Expression l)

derive instance functorExpression :: Functor Expression

instance showExpression :: Show l => Show (Expression l) where
  show expr = "(" <> show (getLocation expr) <> ": " <> printExpression expr <> ")"

-- Prints an expression without it's location. 
-- Only used internally inside the show instance 
-- to not reepat the location printing code every time
printExpression :: forall l. Show l => Expression l -> String
printExpression (Variable _ name) = unwrap name

printExpression (FunctionCall _ f i) = show f <> " " <> show i

printExpression (Lambda _ arg value) = "\\" <> show arg <> " -> " <> show value

printExpression (Literal _ literal) = case literal of
  LInt v -> show v
  LBool b -> show b

printExpression (Let _ name value body) = "let " <> unwrap name <> " = " <> show value <> " in " <> show body

printExpression (If _ c t f) = "if " <> show c <> " then " <> show t <> " else " <> show f

printExpression (FixPoint _ e) = "fixpoint( " <> show e <> " )"

printExpression (Native _ (NativeExpression t _)) = "native :: " <> show t
