module Lunarbox.Component.Editor (component, State, Action(..), Query, Tab) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Array (foldr, (..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Foldable (for_, sequence_)
import Data.Lens (Lens', Traversal', _Just, over, preview, set, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List.Lazy as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (replicate)
import Debug.Trace (trace)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, query, tell)
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
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (printTypeMap, solveExpression)
import Lunarbox.Control.Monad.Effect (print, printString)
import Lunarbox.Data.Dataflow.Class.Expressible (nullExpr)
import Lunarbox.Data.Dataflow.Expression (Expression)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Type (Type, numberOfInputs)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup)
import Lunarbox.Data.Editor.Project (Project, _ProjectFunctions, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction, emptyProject)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Lunarbox.Svg.Attributes (transparent)
import Record as Record
import Svg.Attributes (Color)

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
    , nextId :: Int
    , currentFunction :: Maybe FunctionName
    , typeMap :: Map Location Type
    , colorMap :: Map Location Color
    , expression :: Expression Location
    , lastMousePosition :: Maybe (Vec2 Number)
    , nodeData :: Map (Tuple FunctionName NodeId) NodeData
    , functionData :: Map FunctionName FunctionData
    }

_nodeData :: Lens' State (Map (Tuple FunctionName NodeId) NodeData)
_nodeData = prop (SProxy :: _ "nodeData")

_atNodeData :: FunctionName -> NodeId -> Lens' State (Maybe NodeData)
_atNodeData name id = _nodeData <<< at (Tuple name id)

_project :: Lens' State Project
_project = prop (SProxy :: _ "project")

_colorMap :: Lens' State (Map Location Color)
_colorMap = prop (SProxy :: _ "colorMap")

_atColorMap :: Location -> Traversal' State (Maybe Color)
_atColorMap location = _colorMap <<< at location

_lastMousePosition :: Lens' State (Maybe (Vec2 Number))
_lastMousePosition = prop (SProxy :: _ "lastMousePosition")

_expression :: Lens' State (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: Lens' State (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: Lens' State Int
_nextId = prop (SProxy :: _ "nextId")

_StateProjectFunctions :: Lens' State (G.Graph FunctionName DataflowFunction)
_StateProjectFunctions = _project <<< _ProjectFunctions

_stateProjectNodeGroup :: FunctionName -> Traversal' State NodeGroup
_stateProjectNodeGroup name = _project <<< _projectNodeGroup name

_stateAtProjectNode :: FunctionName -> NodeId -> Traversal' State (Maybe Node)
_stateAtProjectNode name id = _project <<< _atProjectNode name id

_nodeIsSelected :: FunctionName -> NodeId -> Traversal' State Boolean
_nodeIsSelected name id = _atNodeData name id <<< _Just <<< _NodeDataSelected

_StateAtProjectFunction :: FunctionName -> Traversal' State (Maybe DataflowFunction)
_StateAtProjectFunction name = _project <<< _atProjectFunction name

_StateCurrentFunction :: Lens' State (Maybe FunctionName)
_StateCurrentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: Lens' State Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: Lens' State Tab
_currentTab = prop (SProxy :: _ "currentTab")

data Action
  = ChangeTab Tab
  | CreateFunction FunctionName
  | SelectFunction (Maybe FunctionName)
  | CreateNode FunctionName
  | StartFunctionCreation
  | Compile
  | SceneMouseUp
  | SceneMouseDown (Vec2 Number)
  | SceneMouseMove (Vec2 Number)
  | SelectNode NodeId

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

component :: forall m. MonadEffect m => MonadReader Config m => Component HH.HTML Query {} Void m
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
        , expression: nullExpr Nowhere
        , project: loadPrelude $ emptyProject $ NodeId "firstOutput"
        }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Compile
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Compile -> do
      { project, expression } <- get
      let
        expression' = compileProject project
      -- we only run the type inference algorithm if the expression changed
      when (expression /= expression') do
        let
          typeMap = case solveExpression expression' of
            Right map -> Map.delete Nowhere map
            Left _ -> mempty
        printString $ printTypeMap typeMap
        -- printString $ printSource expression'
        -- TODO: make it so this accounts for errors
        modify_ $ Record.merge { expression: expression', typeMap }
    CreateNode name -> do
      print "here:)"
      Tuple id setId <- createId
      typeMap <- gets $ view _typeMap
      maybeCurrentFunction <- gets $ view _StateCurrentFunction
      maybeNodeFunction <- gets $ preview $ _StateAtProjectFunction name
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
                state'' = set (_stateAtProjectNode currentFunction id) (Just node) state'

                state''' = set (_atNodeData currentFunction id) (Just def) state''
              void $ put $ setId state'''
              handleAction Compile
              trace (Map.lookup (Tuple currentFunction id) state'''.nodeData) \_ -> pure unit
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      modify_
        if (oldTab == newTab) then
          over _panelIsOpen not
        else
          set _currentTab newTab
    CreateFunction name -> do
      Tuple id setId <- createId
      let
        setFunction = over _project $ createFunction name id
      modify_ $ setId <<< setFunction
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    SelectFunction name -> do
      -- we need the current function to lookup the function in the function graph
      oldName <- gets $ view _StateCurrentFunction
      functions <- gets $ view _StateProjectFunctions
      -- this is here to update the function the Scene component renders
      when (name /= oldName)
        $ sequence_ do
            currentFunction <- name
            function <-
              G.lookup currentFunction functions
            pure do
              handleAction Compile
              -- And finally, save the selected function in the state
              modify_ $ set _StateCurrentFunction name
    SceneMouseDown position -> do
      modify_ $ set _lastMousePosition $ Just position
    SceneMouseMove position -> do
      state@{ lastMousePosition } <- get
      let
        state' = set _lastMousePosition (Just position) state
      for_ lastMousePosition \oldPosition -> do
        let
          offset = position - oldPosition

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
      maybeCurrentFunction <- gets $ view _StateCurrentFunction
      for_ maybeCurrentFunction \currentFunction -> do
        modify_ $ set (_nodeIsSelected currentFunction id) true

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
  panel { currentTab, project, currentFunction, functionData } = case currentTab of
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
                { functions:
                  (maybe mempty pure currentFunction)
                    <> ( Set.toUnfoldable $ Map.keys $ onlyEditable currentFunction project
                      )
                , selected: currentFunction
                }
                handleTreeOutput
            ]
        ]
    Add ->
      container "panel-container"
        [ container "title" [ HH.text "Add node" ]
        , AddC.add { project, currentFunction, functionData }
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
