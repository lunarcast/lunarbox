module Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret
  ( interpret
  , withTerm
  , normalizeTerm
  ) where

import Prelude
import Control.Monad.Reader (asks, local)
import Control.Monad.Writer (tell)
import Data.Default (class Default, def)
import Data.Lens (over, set, view)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Lunarbox.Control.Monad.Dataflow.Interpreter (Interpreter, _location, _overwrites, _termEnv)
import Lunarbox.Data.Dataflow.Expression (Expression(..), NativeExpression(..), everywhereOnExpressionM, getLocation)
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

-- | Get the underlying value from a term
normalizeTerm :: forall l. Ord l => Default l => Term l -> Interpreter l RuntimeValue
normalizeTerm (Term a) = pure a

normalizeTerm (Code env expr) = result >>= normalizeTerm
  where
  result = withEnv env $ interpret expr

normalizeTerm _ = pure Null

-- | Mark all the places inside an expression as null
markAsNull :: forall l. Ord l => Expression l -> Interpreter l (Term l)
markAsNull expr =
  everywhereOnExpressionM (const $ pure true)
    ( \e -> do
        tell $ ValueMap $ Map.singleton (getLocation e) $ Term Null
        pure e
    )
    expr
    $> def

-- Interpret an expression into a runtimeValue
interpret :: forall l. Ord l => Default l => Expression l -> Interpreter l (Term l)
interpret expression = do
  overwrites <- asks $ view _overwrites
  let
    location = getLocation expression

    maybeOverwrite = Map.lookup location $ unwrap overwrites
  value <- case maybeOverwrite of
    Just overwrite -> pure overwrite
    Nothing ->
      local (set _location location) case expression of
        TypedHole _ -> pure $ Term Null
        Variable _ name -> getVariable $ show name
        Lambda _ argumentName body -> do
          -- This is here to generate data for node uis
          -- void $ withTerm (show argumentName) def $ interpret body
          env <- getEnv
          pure $ Closure env (show argumentName) body
        Chain _ expressions -> case List.last expressions of
          Just expression' -> interpret expression'
          Nothing -> pure def
        If _ cond then' else' -> interpret cond >>= go
          where
          go = case _ of
            Term (Bool true) -> interpret then'
            Term (Bool false) -> interpret else'
            Term (RLazy exec) -> go (Term $ exec unit)
            Term Null -> do
              void $ markAsNull then'
              markAsNull else'
            t -> pure def
        Let _ name value body -> do
          runtimeValue <- interpret value
          withTerm (show name) runtimeValue $ interpret body
        expr@(FixPoint l name body) -> do
          env <- getEnv
          let
            self = Code env expr
          withTerm (show name) self $ interpret body
        Native _ (NativeExpression _ inner) -> pure $ Term inner
        FunctionCall _ function argument -> do
          runtimeArgument <- interpret argument
          runtimeFunction <- interpret function
          let
            go = case _ of
              Closure env name expr ->
                withEnv env $ withTerm name runtimeArgument
                  $ interpret expr
              Term (Function call) -> Term <$> call <$> normalizeTerm runtimeArgument
              Code env expr -> call >>= go
                where
                call = withEnv env $ interpret expr
              Term _ -> pure def
          go runtimeFunction
  tell $ ValueMap $ Map.singleton location value
  pure value
