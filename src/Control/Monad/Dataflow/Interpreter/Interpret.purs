module Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret
  ( interpret
  ) where

import Prelude
import Control.Monad.Reader (asks, local)
import Data.Int (toNumber)
import Data.Lens (over, set, view)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lunarbox.Control.Monad.Dataflow.Interpreter (Interpreter, _termEnv)
import Lunarbox.Data.Dataflow.Expression (Expression(..), Literal(..), NativeExpression(..))
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..), lookup)
import Lunarbox.Data.Lens (newtypeIso)

-- Interpreter for Expressions
type ExpressionInterpreter l
  = Interpreter (Expression l) l

-- Gets a value from the current environment
getVariable :: forall l. Ord l => String -> ExpressionInterpreter l (RuntimeValue (Expression l))
getVariable name = do
  env <- asks $ view _termEnv
  pure $ lookup name env

-- Perform an action in an environment with an extra variable
withTerm :: forall l. Ord l => String -> RuntimeValue (Expression l) -> ExpressionInterpreter l ~> ExpressionInterpreter l
withTerm name value = local $ over (_termEnv <<< newtypeIso) $ Map.insert (show name) value

-- Interpret an expression into a runtimeValue
interpret :: forall l. Ord l => Expression l -> ExpressionInterpreter l (RuntimeValue (Expression l))
interpret = case _ of
  Literal _ (LInt value) -> pure $ Number $ toNumber value
  Literal _ (LBool value) -> pure $ Bool value
  Literal _ LNull -> pure Null
  Variable _ name -> getVariable $ show name
  Lambda _ argument body -> asks $ view _termEnv <#> Closure (show argument) body
  Chain _ expressions -> case List.last expressions of
    Just expression -> interpret expression
    Nothing -> pure Null
  Let _ _ name value body -> do
    runtimeValue <- interpret value
    local (over (_termEnv <<< newtypeIso) $ Map.insert (show name) runtimeValue) $ interpret body
  FixPoint location function -> interpret $ FunctionCall location function $ FixPoint location function
  Native _ (NativeExpression _ call) -> pure call
  FunctionCall _ argument function -> do
    runtimeArgument <- interpret argument
    runtimeFunction <- interpret function
    case runtimeFunction of
      Function call -> pure $ call runtimeArgument
      Closure name expression environment ->
        withTerm name runtimeArgument
          $ local (set _termEnv environment)
          $ interpret expression
      _ -> pure Null
