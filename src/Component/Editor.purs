module Lunarbox.Component.Editor where

import Prelude
import Control.Monad.State (modify)
import Control.MonadZero (guard)
import Data.Graph as G
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, id_)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Node as Node
import Lunarbox.Component.Utils (container)
import Lunarbox.Data.Project (FunctionName(..), NodeGroup(..), NodeId(..), Project, VisualFunction(..), emptyProject)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)

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
    , project :: Project
    , lastId :: Int
    , currentFunction :: FunctionName
    }

data Action
  = ChangeTab Tab

data Query a
  = Void

type ChildSlots
  = ( node :: Slot Node.Query Void Unit
    )

component :: forall m. MonadEffect m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState:
      const
        { currentTab: Settings
        , panelIsOpen: false
        , project: emptyProject $ NodeId $ "firstOutput"
        , lastId: 0
        , currentFunction: FunctionName "nothing"
        }
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

  simulationContent { project, currentFunction } =
    fromMaybe
      (emptyEditor unit) do
      function <- G.lookup currentFunction project.functions
      case function of
        NativeVF _ -> Nothing
        DataflowFunction (NodeGroup { output }) -> Just $ HH.text $ show $ output

  render s@{ currentTab, panelIsOpen } =
    container "editor"
      [ container "sidebar"
          $ tabs currentTab
      , HH.div
          [ id_ "panel", classes $ ClassName <$> (guard panelIsOpen $> "active") ]
          [ panel s ]
      , container "simulation"
          [ simulationContent s ]
      ]
