module Lunarbox.Component.Editor
  ( component
  , Action(..)
  , Query
  ) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Array (foldr, (..))
import Data.Default (def)
import Data.Editor.Foreign.SceneBoundingBox (getSceneBoundingBox)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (over, preview, set, view)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (replicate)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, SubscriptionId, defaultEval, mkComponent, mkEval, query, subscribe, subscribe', tell)
import Halogen.HTML (lazy2)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, id_)
import Halogen.Query.EventSource as ES
import Lunarbox.Component.Editor.Add as AddC
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (container)
import Lunarbox.Config (Config)
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (printTypeMap)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Dataflow.Expression (printSource)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type (numberOfInputs)
import Lunarbox.Data.Editor.Camera (toWorldCoordinates, zoomOn)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (_projectNodeGroup)
import Lunarbox.Data.Editor.Save (jsonToState, stateToJson)
import Lunarbox.Data.Editor.State (State, Tab(..), _atColorMap, _atNode, _atNodeData, _currentCamera, _currentFunction, _currentTab, _expression, _function, _functions, _isSelected, _lastMousePosition, _nextId, _nodeData, _panelIsOpen, _partialFrom, _partialTo, _typeMap, adjustSceneScale, compile, deleteSelection, emptyState, getSceneMousePosition, initializeFunction, pan, removeConnection, setCurrentFunction, setRuntimeValue, setScale, tabIcon, tryConnecting)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.MouseButton (MouseButton(..), isPressed)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Lunarbox.Svg.Attributes (transparent)
import Web.Event.Event (EventType(..))
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Init
  | HandleKey SubscriptionId KeyboardEvent
  | ChangeTab Tab
  | CreateFunction FunctionName
  | SelectFunction (Maybe FunctionName)
  | CreateNode FunctionName
  | StartFunctionCreation
  | SceneMouseUp
  | SceneMouseDown (Vec2 Number)
  | SceneMouseMove Int (Vec2 Number)
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | SelectNode NodeId
  | SceneZoom Number
  | RemoveConnection NodeId (Tuple NodeId Int)
  | SetRuntimeValue FunctionName NodeId RuntimeValue
  | AdjustSceneScale
  | TogglePanel

data Query a
  = Void

type ChildSlots
  = ( tree :: Slot TreeC.Query TreeC.Output Unit
    )

-- Actions to run the scene component with
sceneActions :: Scene.Actions Action
sceneActions =
  { mouseUp: Just SceneMouseUp
  , mouseDown: Just <<< SceneMouseDown
  , zoom: Just <<< SceneZoom
  , selectNode: Just <<< SelectNode
  , selectOutput: Just <<< SelectOutput
  , mouseMove: (Just <<< _) <<< SceneMouseMove
  , selectInput: (Just <<< _) <<< SelectInput
  , removeConnection: (Just <<< _) <<< RemoveConnection
  , setValue: ((Just <<< _) <<< _) <<< SetRuntimeValue
  }

-- This is a helper monad which just generates an id
createId :: forall m. HalogenM (State Action ChildSlots m) Action ChildSlots Void m (Tuple NodeId (State Action ChildSlots m -> State Action ChildSlots m))
createId = do
  { nextId } <- get
  pure $ Tuple (NodeId $ show nextId) $ over _nextId (_ + 1)

component :: forall m. MonadAff m => MonadEffect m => MonadReader Config m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState: const $ initializeFunction (FunctionName "main") $ loadPrelude emptyState
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM (State Action ChildSlots m) Action ChildSlots Void m Unit
  handleAction = case _ of
    Init -> do
      window <- liftEffect Web.window
      document <- liftEffect $ Web.document window
      -- Stuff which we need to run at the start
      handleAction AdjustSceneScale
      -- Register keybindings
      subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
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
      | KE.key event == "Delete" || KE.key event == "Backspace" -> do
        modify_ deleteSelection
      | KE.ctrlKey event && KE.key event == "b" -> do
        handleAction TogglePanel
      | KE.ctrlKey event && KE.key event == "Enter" -> do
        state <- get
        let
          json = stateToJson state

          result = jsonToState json
        state' <- case result of
          Left err -> do
            printString err
            pure state
          Right state'' -> pure state''
        put state'
      | otherwise -> pure unit
    CreateNode name -> do
      Tuple id setId <- createId
      typeMap <- gets $ view _typeMap
      maybeCurrentFunction <- gets $ view _currentFunction
      maybeNodeFunction <- gets $ preview $ _function name
      for_ (join maybeNodeFunction) \function -> do
        state <- get
        let
          inputCount = fromMaybe 0 $ numberOfInputs <$> Map.lookup (Location name) typeMap

          inputs = (DeepLocation name <<< DeepLocation id <<< InputPin) <$> 0 .. (inputCount - 1)

          state' =
            foldr
              ( \location ->
                  set (_atColorMap location) $ Just transparent
              )
              state
              inputs

          node :: Node
          node =
            ComplexNode
              { inputs: replicate inputCount Nothing
              , function: name
              }
        for_ maybeCurrentFunction
          $ \currentFunction -> do
              let
                state'' = set (_atNode currentFunction id) (Just node) state'

                state''' = set (_atNodeData currentFunction id) (Just def) state''

                state'''' = over _functions (G.insertEdge name currentFunction) state'''
              void $ put $ compile $ setId state''''
    TogglePanel -> do
      bounds <- liftEffect getSceneBoundingBox
      modify_ $ setScale bounds <<< over _panelIsOpen not
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
    SceneMouseDown position -> getSceneMousePosition position >>= put
    SceneMouseMove bits position -> do
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
      modify_ $ over _nodeData $ map $ set _NodeDataSelected false
    SelectNode id -> do
      maybeCurrentFunction <- gets $ view _currentFunction
      for_ maybeCurrentFunction \currentFunction -> do
        modify_ $ set (_isSelected currentFunction id) true
    SelectInput id index -> do
      let
        setTo = set _partialTo $ Just $ Tuple id index
      modify_ $ tryConnecting <<< setTo
    SelectOutput id -> do
      let
        setFrom = set _partialFrom $ Just id
      modify_ $ tryConnecting <<< setFrom
    RemoveConnection from to -> do
      modify_ $ removeConnection from to
    SetRuntimeValue functionName nodeId runtimeValue -> do
      modify_ $ setRuntimeValue functionName nodeId runtimeValue
    SceneZoom amount -> do
      mousePosition <- gets $ view _lastMousePosition
      modify_ $ over _currentCamera $ zoomOn mousePosition amount

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

  panel :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  panel { currentTab, project, currentFunction, functionData, typeMap } = case currentTab of
    Settings ->
      container "panel-container"
        [ container "title" [ HH.text "Project settings" ]
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
      container "panel-container"
        [ container "title" [ HH.text "Add node" ]
        , lazy2 AddC.add { project, currentFunction, functionData, typeMap }
            { edit: Just <<< SelectFunction <<< Just
            , addNode: Just <<< CreateNode
            }
        ]
    _ -> HH.text "not implemented"

  scene :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  scene { project
  , currentFunction: maybeCurrentFunction
  , expression
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
  } =
    fromMaybe
      emptyEditor do
      currentFunction <- maybeCurrentFunction
      group <-
        preview (_projectNodeGroup currentFunction) project
      pure
        $ lazy2 Scene.scene
            { project
            , expression
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
            sceneActions

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
