module Lunarbox.Component.Editor where

import Prelude
import Control.Monad.State (modify)
import Control.MonadZero (guard)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, id_)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (container)

data Tab
  = Settings
  | Add
  | Tree
  | Problems

derive instance eqTab :: Eq Tab

tabIcon :: Tab -> String
tabIcon = case _ of
  Settings -> "settings"
  Add -> "add"
  Tree -> "account_tree"
  Problems -> "error"

type State
  = { currentTab :: Tab
    , panelIsOpen :: Boolean
    }

data Action
  = ChangeTab Tab

data Query a
  = Void

type ChildSlots
  = ()

component :: forall m. MonadEffect m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState: const { currentTab: Settings, panelIsOpen: false }
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
    ChangeTab tab -> do
      void $ modify (\state@{ panelIsOpen: open, currentTab } -> state { currentTab = tab, panelIsOpen = if (currentTab == tab) then not open else open })

  sidebarIcon activeTab current =
    HH.div
      [ classes $ ClassName <$> [ "sidebar-icon" ] <> (guard isActive $> "active")
      , onClick $ const $ Just $ ChangeTab current
      ]
      [ icon iconName ]
    where
    iconName = tabIcon current

    isActive = current == activeTab

  tabs currentTab =
    [ icon Settings
    , icon Add
    , icon Tree
    , icon Problems
    ]
    where
    icon = sidebarIcon currentTab

  panel s@{ currentTab } = case currentTab of
    Settings ->
      HH.div_
        [ container "title" [ HH.text "Project settings" ]
        ]
    _ -> HH.text "not implemented"

  render s@{ currentTab, panelIsOpen } =
    container "editor"
      [ container "sidebar"
          $ tabs currentTab
      , HH.div
          [ id_ "panel", classes $ ClassName <$> (guard panelIsOpen $> "active") ]
          [ panel s ]
      , container "simulation"
          [ HH.text "simulation"
          ]
      ]
