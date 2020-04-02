module Lunarbox.Data.Dataflow.Expression
  ( Literal(..)
  , NativeExpression(..)
  , Expression(..)
  , VarName(..)
  , functionDeclaration
  , getLocation
  , toMap
  , lookup
  , printExpressionAt
  , sumarizeExpression
  , inputs
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

-- Theses are literal values with primitive values
data Literal
  = LInt Int
  | LBool Boolean
  | LNull

derive instance literalEq :: Eq Literal

instance showLiteral :: Show Literal where
  show (LInt i) = show i
  show (LBool b) = show b
  show LNull = "null"

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

-- internal version of inputs which also takes the accumulated count
inputs' :: forall l. Int -> Expression l -> Int
inputs' count = case _ of
  Lambda _ _ body -> inputs' (count + 1) body
  _ -> count

-- Takes an Expression and returns the number of inputs that function has
-- Examples:
--     for "a -> b -> c" the function returns 2
--     for "let a = b in c" the function returns 0
inputs :: forall l. Expression l -> Int
inputs = inputs' 0

-- Typecalss instances
derive instance expressinEq :: Eq l => Eq (Expression l)

derive instance functorExpression :: Functor Expression

instance showExpression :: Show l => Show (Expression l) where
  show expr = "(" <> show (getLocation expr) <> ": " <> printRawExpression show expr <> ")"

-- Prints only the parts of an expression found at a specific location
printExpressionAt :: forall l. Show l => Eq l => l -> Expression l -> String
printExpressionAt location =
  printRawExpression
    ( \expression ->
        if getLocation expression == location then
          printExpressionAt location expression
        else
          "..."
    )

-- Prints an expression and stops when changing locations
sumarizeExpression :: forall l. Show l => Eq l => Expression l -> String
sumarizeExpression expression = printExpressionAt (getLocation expression) expression

-- Prints an expression without it's location. 
-- Uses a custom function to print the recursive Expressions.
-- Only used internally inside the show instance 
-- to not reepat the location printing code every time
printRawExpression :: forall l. Show l => (Expression l -> String) -> Expression l -> String
printRawExpression print = case _ of
  Variable _ name -> unwrap name
  FunctionCall _ f i -> print f <> " " <> print i
  Lambda _ arg value -> "\\" <> show arg <> " -> " <> print value
  Literal _ literal -> show literal
  Let _ name value body -> "let " <> unwrap name <> " = " <> print value <> " in " <> print body
  If _ c t f -> "if " <> print c <> " then " <> print t <> " else " <> print f
  FixPoint _ e -> "fixpoint( " <> print e <> " )"
  Native _ (NativeExpression t _) -> "native :: " <> show t