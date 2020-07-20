module Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret
  ( interpret
  , withTerm
  , termToRuntime
  ) where

import Prelude
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Writer (tell)
import Data.Default (class Default, def)
import Data.Lens (over, set, view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Lunarbox.Control.Monad.Dataflow.Interpreter (Interpreter, InterpreterContext, _overwrites, _termEnv, _toplevel, evalInterpreter)
import Lunarbox.Data.Dataflow.Expression (Expression(..), NativeExpression(..), getLocation)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Dataflow.Runtime.TermEnvironment (Term(..), TermEnvironment)
import Lunarbox.Data.Dataflow.Runtime.TermEnvironment as TermEnvironment
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap(..))
import Lunarbox.Data.Dataflow.Scheme (Scheme(..))
import Lunarbox.Data.Dataflow.Type (typeString)

-- Gets a value from the current environment
getVariable :: forall l. Ord l => String -> Interpreter l (Term l)
getVariable name = do
  env <- asks $ view _termEnv
  pure $ TermEnvironment.lookup name env

-- | We use this to execute closures
withEnv :: forall l a. Ord l => TermEnvironment l -> Interpreter l a -> Interpreter l a
withEnv = local <<< set _termEnv

-- | Get what we curently have in the environment
getEnv :: forall l. Ord l => Interpreter l (TermEnvironment l)
getEnv = asks $ view _termEnv

-- Perform an action in an environment with an extra variable
withTerm :: forall l. Ord l => String -> Term l -> Interpreter l ~> Interpreter l
withTerm name value = local $ over _termEnv $ TermEnvironment.insert name value

-- Wrap a runtime value in an expression
makeNative :: forall l. Ord l => Default l => RuntimeValue -> Expression l
makeNative = Native def <<< NativeExpression (Forall [] typeString)

-- | Run a computation inside something like a lambda
scoped :: forall l a. Ord l => Interpreter l a -> Interpreter l a
scoped = local $ set _toplevel false

-- | Transform a term into a runtime value
termToRuntime :: forall l. Ord l => Default l => InterpreterContext l -> Term l -> RuntimeValue
termToRuntime _ (Term a) = a

termToRuntime _ (Closure env (Lambda _ _ _)) = Null

termToRuntime ctx (Closure env expr) = termToRuntime ctx result
  where
  result = evalInterpreter ctx $ withEnv env $ interpret expr

-- Interpret an expression into a runtimeValue
interpret :: forall l. Ord l => Default l => Expression l -> Interpreter l (Term l)
interpret expression = do
  overwrites <- asks $ view _overwrites
  let
    location = getLocation expression

    maybeOverwrite = Map.lookup location $ unwrap overwrites
  value <- case maybeOverwrite of
    Just overwrite -> pure overwrite
    Nothing -> case expression of
      TypedHole _ -> pure $ Term Null
      Variable _ name -> do
        getVariable $ show name
      Lambda _ _ _ -> do
        env <- getEnv
        pure $ Closure env expression
      Expression _ inner -> interpret inner
      If _ cond then' else' -> interpret cond >>= go
        where
        go = case _ of
          Term (Bool true) -> interpret then'
          Term (Bool false) -> interpret else'
          t -> pure def
      Let _ name value body -> do
        runtimeValue <- interpret value
        withTerm (show name) runtimeValue $ interpret body
      expr@(FixPoint l name body) -> do
        env <- getEnv
        let
          self = Closure env expr
        withTerm (show name) self $ interpret body
      Native _ (NativeExpression _ inner) -> pure $ Term inner
      FunctionCall _ function argument -> do
        runtimeArgument <- interpret argument
        runtimeFunction <- interpret function
        let
          go = case _ of
            Closure env (Lambda _ name expr) ->
              scoped $ withEnv env $ withTerm (show name) runtimeArgument
                $ interpret expr
            Closure env expr -> call >>= go
              where
              call = scoped $ withEnv env $ interpret expr
            Term (Function call) -> do
              ctx <- ask
              pure $ Term $ call $ termToRuntime ctx runtimeArgument
            Term _ -> pure def
        go runtimeFunction
  toplevel <- asks $ view _toplevel
  when toplevel $ tell $ ValueMap $ Map.singleton location value
  pure value
