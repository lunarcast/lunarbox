module Lunarbox.Component.Editor
  ( component
  , Action(..)
  , Output(..)
  , EditorState
  , ChildSlots
  ) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (execState, get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Default (def)
import Data.Foldable (find, foldr, for_)
import Data.Int (toNumber)
import Data.Lens (_Just, over, preview, set, view)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Vec (vec2)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, SubscriptionId, defaultEval, mkComponent, mkEval, query, raise, subscribe, subscribe', tell)
import Halogen.HTML (lazy, lazy2)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties (classes, id_)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lunarbox.Component.Editor.Add as AddC
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Switch (switch)
import Lunarbox.Component.Utils (className, container, whenElem)
import Lunarbox.Config (Config)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (printTypeMap)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Dataflow.Expression (printSource)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Editor.Camera (toWorldCoordinates, zoomOn)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected, _NodeDataZPosition)
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (_projectNodeGroup)
import Lunarbox.Data.Editor.State (State, Tab(..), _atCurrentNodeData, _atInputCount, _currentCamera, _currentFunction, _currentNodes, _currentTab, _expression, _isAdmin, _isExample, _isSelected, _lastMousePosition, _name, _nextId, _nodeData, _nodeSearchTerm, _panelIsOpen, _partialFrom, _partialTo, _sceneScale, _typeMap, _unconnectablePins, adjustSceneScale, compile, createNode, deleteSelection, getSceneMousePosition, initializeFunction, makeUnconnetacbleList, pan, removeConnection, resetNodeOffset, setCurrentFunction, setRuntimeValue, tabIcon, tryConnecting)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.MouseButton (MouseButton(..), isPressed)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Web.Event.Event (EventType(..), preventDefault, stopPropagation)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action
  = Init
  | HandleKey SubscriptionId KeyboardEvent
  | ChangeTab Tab
  | CreateFunction FunctionName
  | SelectFunction (Maybe FunctionName)
  | CreateNode FunctionName
  | StartFunctionCreation
  | SceneMouseUp
  | SceneMouseMove MouseEvent
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | SelectNode NodeId
  | SceneZoom Number
  | RemoveConnection NodeId (Tuple NodeId Int)
  | SetRuntimeValue FunctionName NodeId RuntimeValue
  | AdjustSceneScale
  | TogglePanel
  | ChangeInputCount FunctionName Int
  | SetName String
  | SetExample Boolean
  | SearchNodes String

data Output m
  = Save (EditorState m)

type ChildSlots
  = ( tree :: Slot TreeC.Query TreeC.Output Unit
    )

-- Shorthand for manually passing the types of the actions and child slots
type EditorState m
  = State Action ChildSlots m

-- Actions to run the scene component with
sceneActions :: Scene.Actions Action
sceneActions =
  { mouseUp: Just SceneMouseUp
  , zoom: Just <<< SceneZoom
  , selectNode: Just <<< SelectNode
  , selectOutput: Just <<< SelectOutput
  , mouseMove: Just <<< SceneMouseMove
  , selectInput: (Just <<< _) <<< SelectInput
  , removeConnection: (Just <<< _) <<< RemoveConnection
  , setValue: ((Just <<< _) <<< _) <<< SetRuntimeValue
  }

-- Create a scene with the set of actions above
createScene :: forall m. Scene.Input Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
createScene = Scene.scene sceneActions

-- This is a helper monad which just generates an id
createId :: forall m. HalogenM (EditorState m) Action ChildSlots Void m (Tuple NodeId (State Action ChildSlots m -> State Action ChildSlots m))
createId = do
  { nextId } <- get
  pure $ Tuple (NodeId $ show nextId) $ over _nextId (_ + 1)

component :: forall m q. MonadAff m => MonadEffect m => MonadReader Config m => Component HH.HTML q (EditorState m) (Output m) m
component =
  mkComponent
    { initialState: compile <<< loadPrelude <<< identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM (EditorState m) Action ChildSlots (Output m) m Unit
  handleAction = case _ of
    Init -> do
      window <- liftEffect Web.window
      document <- liftEffect $ Web.document window
      -- Stuff which we need to run at the start
      handleAction AdjustSceneScale
      scale <- gets $ view _sceneScale
      modify_ $ pan $ (_ / 2.0) <$> scale
      -- Register keybindings
      subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
      -- Registr resize events
      void $ subscribe
        $ ES.eventListenerEventSource
            (EventType "resize")
            (Window.toEventTarget window)
            (const $ Just AdjustSceneScale)
    AdjustSceneScale -> adjustSceneScale
    HandleKey sid event
      | KE.key event == "Delete" || (KE.ctrlKey event && KE.key event == "Backspace") -> do
        modify_ deleteSelection
      | KE.ctrlKey event && KE.key event == "b" -> do
        handleAction TogglePanel
      | KE.ctrlKey event && KE.key event == "s" -> do
        liftEffect $ preventDefault $ KE.toEvent event
        liftEffect $ stopPropagation $ KE.toEvent event
        state <- get
        raise $ Save state
      | otherwise -> pure unit
    CreateNode name -> do
      modify_ $ execState $ createNode name
    TogglePanel -> do
      modify_ $ over _panelIsOpen not
      -- Since this modifies the scale of the scene we also update that
      adjustSceneScale
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      if (oldTab == newTab) then
        handleAction TogglePanel
      else
        modify_ $ set _currentTab newTab
    CreateFunction name -> do
      modify_ $ initializeFunction name
    SelectFunction name -> do
      oldFunction <- gets $ view _currentFunction
      modify_ $ setCurrentFunction name
      when (oldFunction == Nothing) adjustSceneScale
      t <- gets $ view _typeMap
      e <- gets $ view _expression
      printString $ printTypeMap t
      printString $ printSource e
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    SceneMouseMove event -> do
      let
        bits = MouseEvent.buttons event

        position = toNumber <$> vec2 (MouseEvent.clientX event) (MouseEvent.clientY event)
      liftEffect $ preventDefault $ MouseEvent.toEvent event
      state@{ lastMousePosition } <- get
      state' <- getSceneMousePosition position
      camera <- gets $ view _currentCamera
      let
        oldPosition = toWorldCoordinates camera lastMousePosition

        newPosition = toWorldCoordinates camera $ view _lastMousePosition state'

        offset = newPosition - oldPosition

        update =
          if isPressed RightButton bits then
            pan offset
          else
            over _nodeData
              $ map \node@(NodeData { selected }) ->
                  if selected then
                    over _NodeDataPosition (_ + offset) node
                  else
                    node
      put $ update state'
    SceneMouseUp -> do
      modify_ $ resetNodeOffset <<< (over _nodeData $ map $ set _NodeDataSelected false)
    SelectNode id -> do
      maybeCurrentFunction <- gets $ view _currentFunction
      nodes <- gets $ preview _currentNodes
      state <- get
      let
        nodeIds = Set.toUnfoldable $ G.keys $ fromMaybe G.emptyGraph nodes

        nodeData = List.catMaybes $ (\nodeId -> join $ preview (_atCurrentNodeData nodeId) state) <$> nodeIds

        zPositions = (view _NodeDataZPosition) <$> nodeData

        zPosition = 1 + foldr max (-1) zPositions
      for_ maybeCurrentFunction \currentFunction -> do
        modify_
          $ set (_atCurrentNodeData id <<< _Just <<< _NodeDataZPosition) zPosition
          <<< set (_isSelected currentFunction id) true
    SelectInput id index -> do
      printString "selecting input"
      unconnectableList <- gets $ view _unconnectablePins
      let
        setTo = set _partialTo $ Just $ Tuple id index

        location = DeepLocation id $ InputPin index

        shouldConnect = isNothing $ find (_ == location) unconnectableList
      when shouldConnect $ modify_ $ tryConnecting <<< makeUnconnetacbleList <<< setTo
    SelectOutput id -> do
      printString "selecting output"
      unconnectableList <- gets $ view _unconnectablePins
      let
        setFrom = set _partialFrom $ Just id

        location = DeepLocation id OutputPin

        shouldConnect = isNothing $ find (_ == location) unconnectableList
      when shouldConnect $ modify_ $ tryConnecting <<< makeUnconnetacbleList <<< setFrom
    RemoveConnection from to -> do
      modify_ $ removeConnection from to
    SetRuntimeValue functionName nodeId runtimeValue -> do
      modify_ $ setRuntimeValue functionName nodeId runtimeValue
    SceneZoom amount -> do
      mousePosition <- gets $ view _lastMousePosition
      modify_ $ over _currentCamera $ zoomOn mousePosition amount
    ChangeInputCount function amount -> do
      modify_ $ set (_atInputCount function) $ Just amount
    SetName name -> modify_ $ set _name name
    SetExample isExample -> do
      -- We only allow editing the example status when we are an admine
      isAdmin <- gets $ view _isAdmin
      when isAdmin $ modify_ $ set _isExample isExample
    SearchNodes input -> do
      printString input
      modify_ $ set _nodeSearchTerm input

  handleTreeOutput :: TreeC.Output -> Maybe Action
  handleTreeOutput = case _ of
    TreeC.CreatedFunction name -> Just $ CreateFunction name
    TreeC.SelectedFunction name -> Just $ SelectFunction name

  sidebarIcon activeTab current =
    HH.div
      [ classes $ ClassName <$> [ "sidebar-icon" ] <> (guard isActive $> "active")
      , onClick $ const $ Just $ ChangeTab current
      ]
      [ icon $ tabIcon current ]
    where
    isActive = current == activeTab

  tabs currentTab =
    [ icon Settings
    , icon Add
    , icon Tree
    ]
    where
    icon = sidebarIcon currentTab

  panel :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  panel { currentTab, project, currentFunction, functionData, typeMap, inputCountMap, name, isExample, isAdmin, nodeSearchTerm } = case currentTab of
    Settings ->
      container "settings"
        [ container "title" [ HH.text "Project settings" ]
        , HH.div [ className "project-setting" ]
            [ HH.div [ className "setting-label" ] [ HH.text "Name:" ]
            , HH.input
                [ HP.value name
                , HP.placeholder "Project name"
                , className "setting-text-input"
                , onValueInput $ Just <<< SetName
                ]
            ]
        , whenElem isAdmin \_ ->
            HH.div [ className "project-setting" ]
              [ HH.div [ className "setting-label" ] [ HH.text "Example:" ]
              , HH.div [ className "setting-switch-input" ]
                  [ switch { checked: isExample, round: true } (Just <<< SetExample)
                  ]
              ]
        ]
    Tree ->
      container
        "tree"
        [ container "tree-top"
            [ container "title" [ HH.text "Explorer" ]
            , HH.div [ onClick $ const $ Just StartFunctionCreation ] [ icon "note_add" ]
            ]
        , HH.slot (SProxy :: _ "tree") unit TreeC.component
            { functions:
              (maybe mempty pure currentFunction)
                <> ( Set.toUnfoldable $ Map.keys $ onlyEditable currentFunction project
                  )
            , selected: currentFunction
            }
            handleTreeOutput
        ]
    Add ->
      container "add-node-container"
        [ container "title" [ HH.text "Add node" ]
        , flip lazy nodeSearchTerm \nodeSearchTerm' ->
            container "node-search-container"
              [ HH.input
                  [ HP.id_ "node-search-input"
                  , HP.placeholder "Search nodes. Eg: add, greater than..."
                  , HP.value nodeSearchTerm'
                  , onValueInput $ Just <<< SearchNodes
                  ]
              , icon "search"
              ]
        , lazy2 AddC.add
            { project
            , currentFunction
            , functionData
            , typeMap
            , inputCountMap
            , nodeSearchTerm
            }
            { edit: Just <<< SelectFunction <<< Just
            , addNode: Just <<< CreateNode
            , changeInputCount: (Just <<< _) <<< ChangeInputCount
            }
        , container "create-input"
            [ HH.button
                [ className "unselectable"
                , onClick $ const $ Just $ CreateNode $ FunctionName "input"
                ]
                [ HH.text "Create input node"
                ]
            ]
        ]

  scene :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  scene { project
  , currentFunction: maybeCurrentFunction
  , typeMap
  , lastMousePosition
  , functionData
  , nodeData
  , colorMap
  , cameras
  , partialConnection
  , valueMap
  , functionUis
  , sceneScale
  , unconnectablePins
  } =
    fromMaybe
      emptyEditor do
      currentFunction <- maybeCurrentFunction
      group <-
        preview (_projectNodeGroup currentFunction) project
      pure
        $ createScene
            { unconnectablePins
            , project
            , typeMap
            , lastMousePosition
            , functionData
            , partialConnection
            , valueMap
            , functionUis
            , scale: sceneScale
            , typeColors: colorMap
            , functionName: currentFunction
            , camera: fromMaybe def $ Map.lookup currentFunction cameras
            , nodeData:
              Map.fromFoldable
                $ (uncurry \(Tuple _ id) value -> Tuple id value)
                <$> ( List.filter (uncurry \(Tuple name _) _ -> name == currentFunction)
                      $ Map.toUnfoldable nodeData
                  )
            }

  render :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  render state@{ currentTab, panelIsOpen } =
    container "editor"
      [ container "sidebar"
          $ tabs currentTab
      , HH.div
          [ id_ "panel", classes $ ClassName <$> (guard panelIsOpen $> "active") ]
          [ panel state ]
      , container "scene"
          [ scene state
          ]
      ]
