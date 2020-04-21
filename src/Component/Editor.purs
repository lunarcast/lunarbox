module Lunarbox.Component.Editor (component, Action(..), Query) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Array (foldr, (..))
import Data.Default (def)
import Data.Either (Either(..))
import Data.Foldable (for_, sequence_)
import Data.Lens (over, preview, set, view)
import Data.List.Lazy as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (replicate)
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
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Type (numberOfInputs)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected)
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (_projectNodeGroup, compileProject, createFunction, emptyProject)
import Lunarbox.Data.Editor.State (State, Tab(..), _atColorMap, _atNode, _atNodeData, _currentFunction, _currentTab, _function, _functions, _isSelected, _lastMousePosition, _nextId, _nodeData, _panelIsOpen, _project, _typeMap, tabIcon)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Lunarbox.Svg.Attributes (transparent)
import Record as Record

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
  | LoadNodes

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
        , project: emptyProject $ NodeId "firstOutput"
        }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just LoadNodes
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    LoadNodes -> do
      modify_ loadPrelude
      handleAction Compile
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
              void $ put $ setId state'''
              handleAction Compile
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
      oldName <- gets $ view _currentFunction
      functions <- gets $ view _functions
      -- this is here to update the function the Scene component renders
      when (name /= oldName)
        $ sequence_ do
            currentFunction <- name
            function <-
              G.lookup currentFunction functions
            pure do
              handleAction Compile
              -- And finally, save the selected function in the state
              modify_ $ set _currentFunction name
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
      maybeCurrentFunction <- gets $ view _currentFunction
      for_ maybeCurrentFunction \currentFunction -> do
        modify_ $ set (_isSelected currentFunction id) true

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
