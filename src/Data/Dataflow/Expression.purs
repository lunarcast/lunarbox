module Lunarbox.Data.Dataflow.Expression
  ( NativeExpression(..)
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
  , removeWrappers
  , everywhereOnExpressionM
  , everywhereOnExpression
  , foldExpression
  , mapExpression
  , references
  , isReferenced
  , unfoldLambda
  ) where

import Prelude
import Control.Monad.Writer (execWriter, tell)
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.List (List(..), foldr, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Scheme (Scheme)
import Lunarbox.Data.String (indent)

-- Names of variables
newtype VarName
  = VarName String

derive instance eqVarName :: Eq VarName

derive instance ordVarName :: Ord VarName

instance showVarName :: Show VarName where
  show = unwrap

derive instance newtypeVarName :: Newtype VarName _

data NativeExpression
  = NativeExpression Scheme RuntimeValue

derive instance eqNativeExpression :: Eq NativeExpression

data Expression l
  = Variable l VarName
  | FunctionCall l (Expression l) (Expression l)
  | Lambda l VarName (Expression l)
  | Let l VarName (Expression l) (Expression l)
  | If l (Expression l) (Expression l) (Expression l)
  | FixPoint l VarName (Expression l)
  | Expression l (Expression l)
  | Native l NativeExpression
  | TypedHole l

derive instance eqExpression :: Eq l => Eq (Expression l)

derive instance functorExpression :: Functor Expression

-- Map every level of an expression in a monadic context
everywhereOnExpressionM :: forall l m. Monad m => (Expression l -> m Boolean) -> (Expression l -> m (Expression l)) -> Expression l -> m (Expression l)
everywhereOnExpressionM filter f = go'
  where
  go continue (FunctionCall loc func arg) = FunctionCall loc <$> continue func <*> continue arg

  go continue (Lambda loc name body) = Lambda loc name <$> continue body

  go continue (Let loc name value body) = Let loc name <$> continue value <*> continue body

  go continue (FixPoint loc name body) = FixPoint loc name <$> continue body

  go continue (Expression loc expr) = Expression loc <$> continue expr

  go continue (If loc cond then' else') =
    If loc <$> continue cond <*> continue then'
      <*> continue else'

  go _ expr = pure expr

  go' expr = do
    expr' <- f expr
    cond <- filter expr'
    if cond then
      go go' expr'
    else
      pure expr'

-- Map every level of an expression
everywhereOnExpression ::
  forall l.
  (Expression l -> Boolean) ->
  (Expression l -> Expression l) -> Expression l -> Expression l
everywhereOnExpression filter mapper =
  unwrap
    <<< everywhereOnExpressionM
        (Identity <<< filter)
        (Identity <<< mapper)

-- | Same as everywhereOnExpression but has no filtering capabilities
mapExpression :: forall l. (Expression l -> Expression l) -> Expression l -> Expression l
mapExpression = everywhereOnExpression (const true)

-- | Accumulate a value running trough all the layers of an expression
foldExpression ::
  forall l m.
  Monoid m =>
  (Expression l -> Boolean) ->
  (Expression l -> m) -> Expression l -> m
foldExpression filter f = execWriter <<< everywhereOnExpressionM (pure <<< filter) (\expr -> tell (f expr) $> expr)

-- Takes a list of argument names and a body and creates the body of a function
functionDeclaration :: forall l. l -> Expression l -> List VarName -> Expression l
functionDeclaration = foldr <<< Lambda

-- Given an Expression extract it's location
getLocation :: forall l. Expression l -> l
getLocation = case _ of
  Variable l _ -> l
  FunctionCall l _ _ -> l
  Lambda l _ _ -> l
  TypedHole l -> l
  Let l _ _ _ -> l
  FixPoint l _ _ -> l
  Native l _ -> l
  Expression l _ -> l
  If l _ _ _ -> l

-- Takes an Expression and transforms it into a map of location -> expression pairs
toMap :: forall l. Ord l => Expression l -> Map.Map l (Expression l)
toMap expression =
  Map.singleton (getLocation expression) expression
    <> case expression of
        FunctionCall _ calee input -> toMap calee <> toMap input
        Lambda _ _ body -> toMap body
        Let _ _ value body -> toMap value <> toMap body
        Expression _ inner -> toMap inner
        FixPoint _ _ body -> toMap body
        If _ cond then' else' -> toMap cond <> toMap then' <> toMap else'
        _ -> mempty

-- get all the locations from an expression
locations :: forall l. Ord l => Expression l -> Set l
locations = Map.keys <<< toMap

-- Tries finding the expression at a certain location
lookup :: forall l. Ord l => l -> Expression l -> Maybe (Expression l)
lookup key = Map.lookup key <<< toMap

-- Takes an Expression and returns the number of inputs that function has
-- Examples:
--     for "a -> b -> c" the function returns 2
--     for "let a = b in c" the function returns 0
inputs :: forall l. Expression l -> Int
inputs = List.length <<< fst <<< unfoldLambda

-- Typecalss instances
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

-- | Unfold a lambda into its args and its return
unfoldLambda :: forall l. Expression l -> Tuple (List VarName) (Expression l)
unfoldLambda (Lambda _ name body) = Tuple (name : rest) return
  where
  (Tuple rest return) = unfoldLambda body

unfoldLambda expr = Tuple Nil expr

-- | Same as unfoldLambda but unfolds Fixpoints operators as well 
unfoldLambda' :: forall l. Expression l -> Tuple (List VarName) (Expression l)
unfoldLambda' (FixPoint loc name body) = unfoldLambda' (Lambda loc name body)

unfoldLambda' (Lambda _ name body) = Tuple (name : rest) return
  where
  (Tuple rest return) = unfoldLambda body

unfoldLambda' expr = Tuple Nil expr

-- Prints an expression without it's location. 
-- Uses a custom function to print the recursive Expressions.
-- Only used internally inside the show instance 
-- to not reepat the location printing code every time
printRawExpression :: forall l. (Expression l -> String) -> Expression l -> String
printRawExpression print expression = case expression of
  Variable _ name -> unwrap name
  FunctionCall _ f i
    | isFunctionCall (removeWrappers i) -> print f <> " (" <> print i <> ")"
  FunctionCall _ f i -> print f <> " " <> print i
  Lambda _ _ _ -> "\\" <> joinWith " " (Array.fromFoldable $ show <$> args) <> " -> " <> print return
    where
    (Tuple args return) = unfoldLambda' expression
  TypedHole _ -> "_"
  Let _ _ _ _ -> printLet true (printRawExpression print) expression
  If _ cond then' else' ->
    joinWith "\n"
      [ "if " <> print cond <> " then"
      , indent 4 (print then')
      , "else"
      , indent 4 (print else')
      ]
  FixPoint loc name e -> print (Lambda loc name e)
  Native _ (NativeExpression t _) -> "native"
  Expression l e -> "(" <> print e <> ")"

-- | Check if an expression is a lambda application
isFunctionCall :: forall l. Expression l -> Boolean
isFunctionCall (FunctionCall _ _ _) = true

isFunctionCall _ = false

-- | Print an expression without the locations
printSource :: forall l. Expression l -> String
printSource = printSource' <<< removeWrappers

-- | Internal version of printSource which doesn't removes Expressionpers
printSource' :: forall l. Expression l -> String
printSource' = printRawExpression (\e -> printSource' e)

-- UnExpression an expression as much as possible
removeWrappers :: forall l. Expression l -> Expression l
removeWrappers = mapExpression go
  where
  go (Expression loc inner) = go inner

  go e = e

-- | Checks if a variable is referenced in an expression
isReferenced :: forall l. VarName -> Expression l -> Boolean
isReferenced target = not <<< Array.null <<< references target

-- | Get all the places a variable is referenced in
references :: forall l. VarName -> Expression l -> Array l
references target = foldExpression filterExpr go
  where
  go (Variable location name)
    | target == name = [ location ]

  go _ = []

  filterExpr (Lambda _ name _)
    | name == target = false

  filterExpr (FixPoint _ name _)
    | name == target = false

  filterExpr (Let _ name _ _)
    | name == target = false

  filterExpr _ = true
