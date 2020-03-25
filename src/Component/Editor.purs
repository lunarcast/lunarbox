module Lunarbox.Component.Editor where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (get, gets, modify_)
import Control.MonadZero (guard)
import Data.Foldable (sequence_, traverse_)
import Data.Int (toNumber)
import Data.Lens (Lens', _1, over, preview, set, view)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, query, request, tell)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, id_)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Editor.Node as Node
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Config (Config)
import Lunarbox.Data.FunctionData (FunctionData, _FunctionDataScale)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.Project (FunctionName, Node(..), NodeId(..), Project, VisualFunction(..), _DataflowFunction, _functions, createFunction, emptyProject, getFunctions)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Svg.Attributes as SA
import Svg.Elements as SE

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
    , project :: Project FunctionData NodeData
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    }

_project :: Lens' State (Project FunctionData NodeData)
_project = prop (SProxy :: _ "project")

_projectFunctions :: Lens' State (G.Graph FunctionName (Tuple (VisualFunction NodeData) FunctionData))
_projectFunctions = _project <<< _functions

_currentFunction :: Lens' State (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: Lens' State Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: Lens' State Tab
_currentTab = prop (SProxy :: _ "currentTab")

data Action
  = ChangeTab Tab
  | CreateFunction FunctionName
  | SelectFunction (Maybe FunctionName)
  | StartFunctionCreation

data Query a
  = Void

type ChildSlots
  = ( scene :: Slot Scene.Query Scene.Output Unit
    , tree :: Slot TreeC.Query TreeC.Output Unit
    , node :: Slot Node.Query Node.Output FunctionName
    )

component :: forall m. MonadEffect m => MonadReader Config m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState:
        const
          { currentTab: Settings
          , panelIsOpen: false
          , project: emptyProject mempty mempty $ NodeId "firstOutput"
          , nextId: 0
          , currentFunction: Nothing
          }
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              }
    }
  where
  createId :: HalogenM State Action ChildSlots Void m NodeId
  createId = do
    { nextId } <- get
    modify_ (_ { nextId = nextId + 1 })
    pure $ NodeId $ show nextId

  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      modify_
        if (oldTab == newTab) then
          over _panelIsOpen not
        else
          set _currentTab newTab
    CreateFunction name -> createId >>= update
      where
      update = modify_ <<< over _project <<< createFunction mempty mempty name
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    SelectFunction name -> do
      -- we need the current function to lookup the function in the function graph
      { project, currentFunction: oldName } <- get
      -- Save the selected function in the state
      modify_ $ set _currentFunction name
      -- this is here to update the function the Scene component renders
      when (name /= oldName)
        $ sequence_ do
            currentFunction <- name
            oldFunction <- oldName
            Tuple function _ <-
              G.lookup currentFunction project.functions
            -- we only render DataflowFunctions
            group <- preview _DataflowFunction function
            let
              functionData =
                query
                  (SProxy :: _ "scene")
                  unit
                  $ request
                  $ Scene.SelectFunction
                  $ Tuple currentFunction group
            pure $ functionData
              >>= traverse_
                  ( modify_
                      <<< set (_projectFunctions <<< ix oldFunction <<< _1 <<< _DataflowFunction)
                  )

  handleTreeOutput :: TreeC.Output -> Maybe Action
  handleTreeOutput = case _ of
    TreeC.CreatedFunction name -> Just $ CreateFunction name
    TreeC.SelectedFunction name -> Just $ SelectFunction name

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

  panel { currentTab, project, currentFunction } = case currentTab of
    Settings ->
      container "panel-container"
        [ container "title" [ HH.text "Project settings" ]
        ]
    Tree ->
      container "panel-container"
        [ container "title" [ HH.text "Explorer" ]
        , container "tree"
            [ container "actions"
                [ HH.hr [ HP.id_ "line" ]
                , HH.div [ onClick $ const $ Just StartFunctionCreation ] [ icon "note_add" ]
                ]
            , HH.slot (SProxy :: _ "tree") unit TreeC.component
                { functions: getFunctions project, selected: currentFunction
                }
                handleTreeOutput
            ]
        ]
    Add ->
      container "panel-container"
        [ container "title" [ HH.text "Add node" ]
        , container "nodes"
            $ makeNode
            <$> G.toUnfoldable project.functions
        ]
      where
      input :: FunctionName -> Node.Input
      input name =
        { selectable: false
        , nodeData: mempty
        , node: ComplexNode { inputs: mempty, function: name }
        }

      makeNode (Tuple name (Tuple function functionData)) =
        let
          scale = view _FunctionDataScale functionData

          side = toNumber $ max (scale !! d0) (scale !! d1)
        in
          HH.div [ className "node" ]
            [ SE.svg
                [ SA.width 75.0
                , SA.height 75.0
                , SA.viewBox 0.0 0.0 side side
                ]
                [ slot
                    (SProxy :: _ "node")
                    name
                    Node.component
                    (input name)
                    absurd
                ]
            , container "node-data"
                [ container "node-text"
                    [ container "node-name"
                        [ HH.text $ show name
                        ]
                    ]
                , container "node-buttons"
                    [ icon "add"
                    , icon "edit"
                    ]
                ]
            ]
    _ -> HH.text "not implemented"

  scene { project, currentFunction: maybeCurrentFunction } =
    fromMaybe
      (emptyEditor unit) do
      currentFunction <- maybeCurrentFunction
      Tuple function _ <- G.lookup currentFunction project.functions
      case function of
        NativeVF _ -> Nothing
        DataflowFunction group ->
          pure
            $ HH.slot
                (SProxy :: _ "scene")
                unit
                Scene.component
                { project, function: Tuple currentFunction group }
                absurd

  render state@{ currentTab, panelIsOpen, project, currentFunction } =
    container "editor"
      [ container "sidebar"
          $ tabs currentTab
      , HH.div
          [ id_ "panel", classes $ ClassName <$> (guard panelIsOpen $> "active") ]
          [ panel state ]
      , container "scene"
          [ scene { project, currentFunction }
          ]
      ]
