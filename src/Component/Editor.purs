module Lunarbox.Component.Editor (component, State, Action(..), Query, Tab) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (get, gets, modify_)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Foldable (for_, sequence_, traverse_)
import Data.Lens (Lens', Traversal', over, preview, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, query, tell)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, id_)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Editor.Add as AddC
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (container)
import Lunarbox.Config (Config)
import Lunarbox.Data.Dataflow.FunctionName (FunctionName)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.NodeId (NodeId(..))
import Lunarbox.Data.FunctionData (FunctionData)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.NodeData (NodeData)
import Lunarbox.Data.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Project (DataflowFunction, Node(..), NodeGroup, Project, _atProjectNode, _functions, _projectNodeGroup, createFunction, emptyProject)
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
    , project :: Project FunctionData NodeData
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    }

_project :: Lens' State (Project FunctionData NodeData)
_project = prop (SProxy :: _ "project")

_projectFunctions :: Lens' State (G.Graph FunctionName (Tuple (DataflowFunction NodeData) FunctionData))
_projectFunctions = _project <<< _functions

_stateProjectNodeGroup :: FunctionName -> Traversal' State (NodeGroup NodeData)
_stateProjectNodeGroup name = _project <<< _projectNodeGroup name

_stateAtProjectNode :: FunctionName -> NodeId -> Traversal' State (Maybe (Tuple Node NodeData))
_stateAtProjectNode name id = _project <<< _atProjectNode name id

_currentFunction :: Lens' State (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: Lens' State Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: Lens' State Tab
_currentTab = prop (SProxy :: _ "currentTab")

data Action
  = ChangeTab Tab
  | CreateFunction FunctionName
  | StartFunctionCreation
  | SyncProjectData
  | SelectFunction (Maybe FunctionName)
  | CreateNode FunctionName
  | UpdateNodeGroup (NodeGroup NodeData)

data Query a
  = Void

type ChildSlots
  = ( scene :: Slot Scene.Query Scene.Output Unit
    , tree :: Slot TreeC.Query TreeC.Output Unit
    , add :: Slot AddC.Query AddC.Output Unit
    )

component :: forall m. MonadEffect m => MonadReader Config m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState:
        const
          { currentTab: Settings
          , panelIsOpen: false
          , project: loadPrelude $ emptyProject mempty mempty $ NodeId "firstOutput"
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
    UpdateNodeGroup group -> do
      (gets $ view _currentFunction)
        >>= traverse_ \currentFunction ->
            modify_
              $ set (_stateProjectNodeGroup currentFunction) group
    CreateNode name -> do
      handleAction SyncProjectData
      id <- createId
      maybeCurrentFunction <- gets $ view _currentFunction
      let
        node :: Node
        node = ComplexNode { inputs: mempty, function: name }
      for_ maybeCurrentFunction
        $ \currentFunction ->
            modify_
              $ set (_stateAtProjectNode currentFunction id)
              $ Just
              $ Tuple node mempty
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      modify_
        if (oldTab == newTab) then
          over _panelIsOpen not
        else
          set _currentTab newTab
    SyncProjectData -> do
      void $ query (SProxy :: _ "scene") unit $ tell Scene.BeforeFunctionChanging
    CreateFunction name -> do
      id <- createId
      modify_ $ over _project $ createFunction mempty mempty name id
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    SelectFunction name -> do
      -- we need the current function to lookup the function in the function graph
      { project, currentFunction: oldName } <- get
      -- this is here to update the function the Scene component renders
      when (name /= oldName)
        $ sequence_ do
            currentFunction <- name
            Tuple function _ <-
              G.lookup currentFunction project.functions
            pure do
              handleAction SyncProjectData
              -- And finally, save the selected function in the state
              modify_ $ set _currentFunction name

  handleTreeOutput :: TreeC.Output -> Maybe Action
  handleTreeOutput = case _ of
    TreeC.CreatedFunction name -> Just $ CreateFunction name
    TreeC.SelectedFunction name -> Just $ SelectFunction name

  handleAddOutput :: AddC.Output -> Maybe Action
  handleAddOutput = case _ of
    AddC.SelectedFunction name -> Just $ SelectFunction $ Just name
    AddC.AddedNode name -> Just $ CreateNode name

  handleSceneOutput :: Scene.Output -> Maybe Action
  handleSceneOutput = case _ of
    Scene.SaveNodeGroup group -> Just $ UpdateNodeGroup group

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
                { functions: Array.toUnfoldable $ (\(Tuple { name } _) -> name) <$> onlyEditable currentFunction project, selected: currentFunction
                }
                handleTreeOutput
            ]
        ]
    Add ->
      container "panel-container"
        [ container "title" [ HH.text "Add node" ]
        , slot (SProxy :: _ "add") unit AddC.component { project, currentFunction } handleAddOutput
        ]
    _ -> HH.text "not implemented"

  scene { project, currentFunction: maybeCurrentFunction } =
    fromMaybe
      (emptyEditor unit) do
      currentFunction <- maybeCurrentFunction
      group <-
        preview (_projectNodeGroup currentFunction) project
      pure
        $ HH.slot
            (SProxy :: _ "scene")
            unit
            Scene.component
            { project, function: Tuple currentFunction group }
            handleSceneOutput

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
