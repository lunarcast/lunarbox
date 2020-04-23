module Lunarbox.Data.Dataflow.Expression
  ( Literal(..)
  , NativeExpression(..)
  , Expression(..)
  , VarName(..)
  , functionDeclaration
  , getLocation
  , toMap
  , locations
  , lookup
  , printExpressionAt
  , printRawExpression
  , printSource
  , sumarizeExpression
  , inputs
  , wrap
  , optimize
  , removeWrappers
  , wrapWith
  , wrappers
  ) where

import Prelude
import Data.List (List(..), foldr, (:))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Scheme (Scheme)
import Lunarbox.Data.String (indent)

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
  = NativeExpression Scheme RuntimeValue

derive instance eqNativeExpression :: Eq NativeExpression

data Expression l
  = Variable l VarName
  | FunctionCall l (Expression l) (Expression l)
  | Lambda l VarName (Expression l)
  | Literal l Literal
  | Let l VarName (Expression l) (Expression l)
  | If l (Expression l) (Expression l) (Expression l)
  | FixPoint l (Expression l)
  | Chain l (List (Expression l))
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
  Chain l _ -> l

-- Takes an Expression and transforms it into a map of location -> expression pairs
toMap :: forall l. Ord l => Expression l -> Map.Map l (Expression l)
toMap expression =
  Map.singleton (getLocation expression) expression
    <> case expression of
        FunctionCall _ calee input -> toMap calee <> toMap input
        Lambda _ _ body -> toMap body
        Let _ _ value body -> toMap value <> toMap body
        If _ condition then' else' -> toMap condition <> toMap then' <> toMap else'
        Chain _ expressions -> foldr (\expression' -> (<>) $ toMap expression') mempty expressions
        FixPoint _ body -> toMap body
        _ -> mempty

-- get all the locations from an expression
locations :: forall l. Ord l => Expression l -> Set l
locations = Map.keys <<< toMap

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
sumarizeExpression = printRawExpression $ const "..."

printRawLet :: forall l. (Expression l -> String) -> Expression l -> String
printRawLet print (Let _ name value _) = indent 2 (unwrap name <> " = " <> print value) <> "\n"

printRawLet _ _ = ""

printLet :: forall l. Boolean -> (Expression l -> String) -> Expression l -> String
printLet true print expression@(Let _ _ _ _) = "let\n" <> printLet false print expression

printLet false print expression@(Let _ _ _ next@(Let _ _ _ _)) = printRawLet print expression <> printLet false print next

printLet false print expression@(Let _ _ _ next) = printRawLet print expression <> "in\n" <> indent 2 (print next)

printLet _ _ _ = ""

-- Prints an expression without it's location. 
-- Uses a custom function to print the recursive Expressions.
-- Only used internally inside the show instance 
-- to not reepat the location printing code every time
printRawExpression :: forall l. Show l => (Expression l -> String) -> Expression l -> String
printRawExpression print expression = case expression of
  Variable _ name -> unwrap name
  FunctionCall _ f i -> print f <> " " <> print i
  Lambda _ arg value -> "\\" <> show arg <> " -> " <> print value
  Literal _ literal -> show literal
  Let _ _ _ _ -> printLet true (printRawExpression print) expression
  FixPoint _ e -> "fixpoint( " <> print e <> " )"
  If _ cond then' else' ->
    "if\n"
      <> indent 2 (print cond)
      <> "\nthen\n"
      <> indent 2 (print then')
      <> "\nelse\n"
      <> indent 2 (print else')
  Native _ (NativeExpression t _) -> "native :: " <> show t
  Chain l (e : Nil) -> printRawExpression print e
  Chain l (e : es) -> "{" <> printRawExpression print e <> "," <> (printRawExpression print $ Chain l es) <> "}"
  Chain _ Nil -> ""

-- Print an expression without the locations
printSource :: forall l. Show l => Expression l -> String
printSource = printRawExpression (\e -> printSource e)

-- Wrap an expression in another expression with a custom location
wrap :: forall l. l -> Expression l -> Expression l
wrap location = Chain location <<< pure

-- Unwrap an expression as much as possible
removeWrappers :: forall l. Expression l -> Expression l
removeWrappers (Chain _ (expression : Nil)) = removeWrappers expression

removeWrappers expression = expression

-- Collect all the locations something is wrapped in
wrappers :: forall l. Expression l -> List l
wrappers (Chain location (expression : Nil)) = location : wrappers expression

wrappers _ = Nil

-- Wrap an expression with a list of locations
wrapWith :: forall l. List l -> Expression l -> Expression l
wrapWith (location : locations') = wrapWith locations' <<< wrap location

wrapWith Nil = identity

-- Optimize an expression
optimize :: forall l. Expression l -> Expression l
optimize expression@(Let location name value body) = case removeWrappers body of
  Variable location' name'
    | name == name' -> wrapWith (wrappers body) $ wrap location' $ optimize value
  _ -> Let location name (optimize value) $ optimize body

optimize (FunctionCall location calee argument) = FunctionCall location (optimize calee) $ optimize argument

optimize (Lambda location argument body) = Lambda location argument $ optimize body

optimize (If location condition then' else') = If location (optimize condition) (optimize then') $ optimize else'

optimize (FixPoint location body) = FixPoint location $ optimize body

optimize (Chain location expressions) = Chain location $ optimize <$> expressions

optimize expression = expression
