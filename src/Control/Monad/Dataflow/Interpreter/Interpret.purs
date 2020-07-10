module Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret
  ( interpret
  , withTerm
  , normalizeTerm
  ) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Lazy (defer)
import Control.Monad.Reader (ask, asks, local)
import Control.Monad.Writer (tell)
import Data.Default (class Default, def)
import Data.Lens (over, set, view)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Debug.Trace (trace)
import Lunarbox.Control.Monad.Dataflow.Interpreter (Interpreter, _location, _overwrites, _termEnv, runInterpreter)
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

-- | Get the underlying value from a term
normalizeTerm :: forall l. Ord l => Default l => Term l -> Interpreter l RuntimeValue
normalizeTerm (Term a) = pure a

normalizeTerm (Code env expr) = result >>= normalizeTerm
  where
  result = withEnv env $ interpret expr

normalizeTerm (Closure env name body) = do
  currentEnv <- ask
  pure
    $ Function \arg ->
        fst $ runInterpreter currentEnv $ bindFlipped normalizeTerm $ withEnv env
          $ withTerm name (Term arg)
          $ interpret body

normalizeTerm (LazyTerm t) = normalizeTerm $ t unit

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
          void $ withTerm (show argumentName) def $ interpret body
          env <- getEnv
          pure $ Closure env (show argumentName) body
        Chain _ expressions -> case List.last expressions of
          Just expression' -> interpret expression'
          Nothing -> pure def
        If _ cond then' else' -> interpret cond >>= go
          where
          go =
            normalizeTerm
              >=> case _ of
                  Bool true -> interpret then'
                  Bool false -> interpret else'
                  RLazy exec -> go (Term $ exec unit)
                  t -> trace { t } \_ -> pure def
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
          case runtimeFunction of
            Closure env name expr ->
              withEnv env $ withTerm name runtimeArgument
                $ interpret expr
            other -> do
              arg <- normalizeTerm runtimeArgument
              function' <- normalizeTerm other
              pure
                $ defer \_ ->
                    Term case function' of
                      Function call -> call arg
                      _ -> trace { arg, function' } \_ -> Null
  tell $ ValueMap $ Map.singleton location value
  pure value
