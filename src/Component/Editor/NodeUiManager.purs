module Lunarbox.Component.Editor.NodeUiManager
  ( Query(..)
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen (Component, HalogenM, defaultEval, get, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Component.Editor.NodeUi (runNodeUi, uiToRuntime)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Editor.FunctionName (FunctionName)

type State
  = { | Input () }

data Action
  = SetValue RuntimeValue

type ChildSlots
  = ()

type Input r
  = ( name :: FunctionName, value :: RuntimeValue | r )

data Query a
  = GetValue (RuntimeValue -> a)

component :: forall m o. Component HH.HTML Query { | Input () } o m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    SetValue val -> modify_ _ { value = val }

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    GetValue return -> get <#> \{ name, value } -> return <$> uiToRuntime name value

  render { name, value } =
    runNodeUi
      { value, setValue: Just <<< SetValue
      }
      name
