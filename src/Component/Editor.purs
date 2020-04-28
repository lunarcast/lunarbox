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
import Halogen (ClassName(..), Component, HalogenM, Slot, SubscriptionId, defaultEval, mkComponent, mkEval, query, subscribe', tell)
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
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Dataflow.Expression (printSource)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Type (numberOfInputs)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), nothing)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (_projectNodeGroup, emptyProject)
import Lunarbox.Data.Editor.State (State, Tab(..), _atColorMap, _atNode, _atNodeData, _currentFunction, _currentTab, _expression, _function, _functions, _isSelected, _lastMousePosition, _nextId, _nodeData, _panelIsOpen, _partialFrom, _partialTo, _typeMap, compile, deleteSelection, getSceneMousePosition, initializeFunction, removeConnection, setCurrentFunction, tabIcon, tryConnecting)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Lunarbox.Svg.Attributes (transparent)
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
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
  | SceneMouseMove (Vec2 Number)
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | SelectNode NodeId
  | LoadNodes
  | RemoveConnection NodeId (Tuple NodeId Int)

data Query a
  = Void

type ChildSlots
  = ( tree :: Slot TreeC.Query TreeC.Output Unit
    )

-- This is a helper monad which just generates an id
createId :: forall m. HalogenM State Action ChildSlots Void m (Tuple NodeId (State -> State))
createId = do
  { nextId } <- get
  pure $ Tuple (NodeId $ show nextId) $ over _nextId (_ + 1)

component :: forall m. MonadAff m => MonadEffect m => MonadReader Config m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState:
      const
        { currentTab: Settings
        , nextId: 0
        , panelIsOpen: false
        , typeMap: mempty
        , colorMap: mempty
        , functionData: mempty
        , nodeData: Map.singleton (Tuple (FunctionName "main") $ NodeId "firstOutput") def
        , currentFunction: Nothing
        , lastMousePosition: Nothing
        , expression: nothing
        , project: emptyProject $ NodeId "firstOutput"
        , partialConnection: def
        , valueMap: mempty
        }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Init -> do
      handleAction LoadNodes
      document <- liftEffect $ Web.document =<< Web.window
      -- Register keybindings
      subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
    HandleKey sid event
      | KE.key event == "Delete" || KE.key event == "Backspace" -> do
        modify_ deleteSelection
      | KE.ctrlKey event && KE.key event == "b" -> do
        modify_ $ over _panelIsOpen not
      | otherwise -> pure unit
    LoadNodes -> do
      modify_ $ compile <<< loadPrelude
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
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      modify_
        if (oldTab == newTab) then
          over _panelIsOpen not
        else
          set _currentTab newTab
    CreateFunction name -> do
      modify_ $ initializeFunction name
    SelectFunction name -> modify_ $ setCurrentFunction name
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    SceneMouseDown position -> getSceneMousePosition position >>= put
    SceneMouseMove position -> do
      state@{ lastMousePosition } <- get
      state' <- getSceneMousePosition position
      let
        relativePosition = view _lastMousePosition state'

        maybeOffset = (-) <$> relativePosition <*> lastMousePosition
      for_ maybeOffset \offset -> do
        let
          updateState =
            over _nodeData
              $ map \node@(NodeData { selected }) ->
                  if selected then
                    over _NodeDataPosition (_ + offset) node
                  else
                    node
        put $ updateState state'
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
      e <- gets $ view _expression
      printString $ printSource e
    RemoveConnection from to -> do
      modify_ $ removeConnection from to

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

  panel :: State -> HH.HTML _ Action
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
        , AddC.add { project, currentFunction, functionData, typeMap }
            { edit: Just <<< SelectFunction <<< Just
            , addNode: Just <<< CreateNode
            }
        ]
    _ -> HH.text "not implemented"

  scene :: forall h. State -> HH.HTML h Action
  scene { project
  , currentFunction: maybeCurrentFunction
  , expression
  , typeMap
  , lastMousePosition
  , functionData
  , nodeData
  , colorMap
  , partialConnection
  , valueMap
  } =
    fromMaybe
      emptyEditor do
      currentFunction <- maybeCurrentFunction
      group <-
        preview (_projectNodeGroup currentFunction) project
      pure
        $ Scene.scene
            { project
            , functionName: currentFunction
            , nodeGroup: group
            , typeColors: colorMap
            , expression
            , typeMap
            , lastMousePosition
            , functionData
            , partialConnection
            , valueMap
            , nodeData:
              Map.fromFoldable
                $ (uncurry \(Tuple _ id) value -> Tuple id value)
                <$> ( List.filter (uncurry \(Tuple name _) _ -> name == currentFunction)
                      $ Map.toUnfoldable nodeData
                  )
            }
            { mouseDown: Just <<< SceneMouseDown
            , mouseMove: Just <<< SceneMouseMove
            , mouseUp: Just SceneMouseUp
            , selectNode: Just <<< SelectNode
            , selectInput: (Just <<< _) <<< SelectInput
            , selectOutput: Just <<< SelectOutput
            , removeConnection: (Just <<< _) <<< RemoveConnection
            }

  render :: State -> HH.HTML _ Action
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
