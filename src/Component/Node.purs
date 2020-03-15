module Lunarbox.Component.Node where

import Prelude
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH

type State
  = { selected :: Boolean
    }

data Action
  = Select
  | Unselect

data Query a
  = Void

type ChildSlots
  = ()

component :: forall m. MonadEffect m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState: const { selected: false }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    _ -> pure unit

  render { selected } = HH.text $ show $ selected
