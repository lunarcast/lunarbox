module Lunarbox.Data.Editor.State where

import Prelude
import Color (cssStringRGBA)
import Control.Monad.State (class MonadState, execState, execStateT, get, gets, put, runState)
import Control.Monad.State as StateM
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Default (def)
import Data.Filterable (filter, filterMap)
import Data.Foldable (foldMap, foldr, length, traverse_)
import Data.Lens (Lens', Traversal', _Just, is, lens, over, preview, set, traversed, view)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Nullable as Nullable
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd, uncurry)
import Data.Unfoldable (replicate)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM, liftEffect, modify_)
import Lunarbox.Capability.Editor.Type (generateColorMap, inputNodeType, typeToColor)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext(..), runInterpreter)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (interpret, termToRuntime)
import Lunarbox.Control.Monad.Dataflow.Solve (SolveState(..))
import Lunarbox.Control.Monad.Dataflow.Solve.SolveExpression (solveExpression)
import Lunarbox.Control.Monad.Dataflow.Solve.Unify (canUnify)
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Dataflow.Expression (Expression(..))
import Lunarbox.Data.Dataflow.Expression.Lint (LintError, lint)
import Lunarbox.Data.Dataflow.Expression.Optimize (dce, inline)
import Lunarbox.Data.Dataflow.Runtime.TermEnvironment (Term)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap(..))
import Lunarbox.Data.Dataflow.Type (Type(..), inputs, multiArgumentFuncion, typeBool)
import Lunarbox.Data.Dataflow.TypeError (TypeError)
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _VisualFunction)
import Lunarbox.Data.Editor.FunctionData (FunctionData(..), _FunctionDataInputs, internal)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location(..), _Function)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode, _nodeInput, _nodeInputs, getFunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..), ScopedLocation(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..), _NodeGroupInputs, _NodeGroupNodes, _NodeGroupOutput)
import Lunarbox.Data.Editor.Project (Project(..), _ProjectFunctions, _ProjectMain, _atProjectFunction, _atProjectNode, _projectNodeGroup, compileProject, createFunction)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Data.Tab (Tab(..))
import Lunarbox.Foreign.Render (GeometryCache, ForeignTypeMap, emptyGeometryCache)
import Lunarbox.Foreign.Render as Native
import Record as Record
import Type.Row (type (+))
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

-- | The result of compiling and typechecking the program
type CompilationResult r
  = ( expression :: Expression Location
    , typeMap :: Map Location Type
    , typeErrors :: Array (TypeError Location)
    , lintingErrors :: Array (LintError Location)
    | r
    )

type StatePermanentData r
  = ( project :: Project
    , nextId :: Int
    , geometries :: Map FunctionName GeometryCache
    , runtimeOverwrites :: ValueMap Location
    , currentFunction :: FunctionName
    | r
    )

-- | The state of the entire editor
type State
  = { 
    | StatePermanentData
      + CompilationResult
      + ( currentTab :: Tab
      , functionData :: Map FunctionName FunctionData
      , panelIsOpen :: Boolean
      , valueMap :: ValueMap Location
      , inputCountMap :: Map FunctionName Int
      , pendingConnection ::
        Maybe
          { from :: NodeId
          , toId :: NodeId
          , toIndex :: Int
          , compilationResult :: { | CompilationResult () }
          }
      , name :: String
      , isExample :: Boolean
      , isAdmin :: Boolean
      , nodeSearchTerm :: String
      , isVisible :: Boolean
      , currentlyEditedNode :: Maybe NodeId
      , autosaveInterval :: Maybe Milliseconds
      , emitInterval :: Maybe Milliseconds
      )
    }

-- Starting state which contains nothing
emptyState :: forall f. MonadEffect f => f State
emptyState =
  initializeFunction (FunctionName "main")
    { currentTab: Settings
    , currentFunction: FunctionName "main"
    , nextId: 0
    , panelIsOpen: false
    , typeMap: mempty
    , functionData: mempty
    , valueMap: mempty
    , runtimeOverwrites: mempty
    , inputCountMap: mempty
    , geometries: mempty
    , expression: TypedHole UnknownLocation
    , project: Project { main: FunctionName "main", functions: mempty }
    , name: "Unnamed project"
    , nodeSearchTerm: ""
    , isExample: false
    , isAdmin: false
    , isVisible: false
    , pendingConnection: Nothing
    , typeErrors: []
    , lintingErrors: []
    , currentlyEditedNode: Nothing
    , autosaveInterval: Just $ Milliseconds 3000.0
    , emitInterval: Nothing
    }

-- Helpers
-- Generate an id and icncrease the inner counter in the state
createId :: StateM.State State NodeId
createId = do
  nextId <- gets $ view _nextId
  modify_ $ over _nextId (_ + 1)
  pure $ NodeId $ show nextId

-- TODO: make it so you don't have to pass exactly this name to createNode to create an input node
inputNodeName :: FunctionName
inputNodeName = FunctionName "input"

-- 
-- Update the way a node looks to fit the latest data
updateNode :: forall m. MonadState State m => MonadEffect m => NodeId -> m Unit
updateNode id =
  gets (view _currentGeometryCache)
    >>= traverse_ \cache ->
        withCurrentFunction \currentFunction ->
          gets (preview $ _atCurrentNode id)
            >>= traverse_ \node -> do
                { typeMap, valueMap, runtimeOverwrites } <- get
                nodeGroup <- gets $ preview (_nodeGroup currentFunction)
                let
                  ctx =
                    over _Newtype
                      _ { overwrites = runtimeOverwrites }
                      (def :: InterpreterContext Location)

                  value =
                    Nullable.toNullable
                      $ (show <<< termToRuntime ctx)
                      <$> getNodeValue currentFunction valueMap id node

                  inputs = List.toUnfoldable $ view _nodeInputs node

                  colorMap = case node of
                    InputNode ->
                      fromMaybe mempty do
                        group <- nodeGroup
                        ty <- Map.lookup (AtFunction currentFunction) typeMap
                        pure $ typeToColor <$> inputNodeType group id ty
                    _ -> generateColorMap (\pin -> flip Map.lookup typeMap $ InsideFunction currentFunction $ PinLocation id pin) node
                liftEffect
                  $ Native.refreshInputArcs cache id
                      { inputs: Nullable.toNullable <$> inputs
                      , value
                      , colorMap:
                        foldr (uncurry go)
                          { output: Nullable.null
                          , inputs: replicate (length inputs) Nullable.null
                          }
                          (Map.toUnfoldable colorMap :: Array _)
                      }
  where
  go OutputPin color map = map { output = Nullable.notNull $ cssStringRGBA color }

  go (InputPin index) color map =
    map
      { inputs =
        fromMaybe map.inputs
          $ Array.updateAt index (Nullable.notNull $ cssStringRGBA color) map.inputs
      }

--
-- Create a new node and save all the data in the different required places
createNode :: forall m. MonadEffect m => MonadState State m => FunctionName -> m Unit
createNode name = do
  let
    isInput = name == inputNodeName

    create = do
      state <- get
      id <- (NodeId (show name <> "-") <> _) <$> createId
      desiredInputCount <- gets $ preview $ _atInputCount name
      let
        maxInputs =
          if isInput then
            0
          else
            getMaxInputs name state

        inputCount = fromMaybe maxInputs $ join desiredInputCount

        node =
          if isInput then
            InputNode
          else
            ComplexNode
              { inputs: replicate inputCount Nothing
              , function: name
              }
      withCurrentFunction_ \currentFunction -> do
        modify_ $ over (_nodes currentFunction) $ Map.insert id node
        when isInput do
          modify_
            $ over (_atFunctionData currentFunction <<< _Just <<< _FunctionDataInputs)
                (_ <> [ { name: "input", description: "The input of a custom function" } ])
          modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ (_ <> pure id)
        modify_ compile
      pure $ Tuple id inputCount
  Tuple (Tuple id inputs) newState <- gets $ runState create
  gets (view _currentGeometryCache)
    >>= traverse_ \cache -> do
        let
          displayName = if isInput then Nullable.null else Nullable.notNull $ show name
        liftEffect $ Native.createNode cache id inputs true displayName
        void $ put newState
        updateNode id

-- Get the type of the output a node (Also works for input pins)
getOutputType :: FunctionName -> NodeId -> State -> Maybe Type
getOutputType functionName id state = do
  let
    typeMap = view _typeMap state
  nodeGroup <- preview (_nodeGroup functionName) state
  currentFunctionType <- Map.lookup (AtFunction functionName) typeMap
  let
    inputIndex = List.findIndex (_ == id) $ view _NodeGroupInputs nodeGroup
  case inputIndex of
    Just index -> (inputs currentFunctionType) `List.index` index
    Nothing -> Map.lookup (InsideFunction functionName $ PinLocation id OutputPin) typeMap

-- Generate the list of inputs can't be connected 
generateUnconnectableInputs :: NodeId -> State -> Set.Set { id :: NodeId, index :: Int }
generateUnconnectableInputs output state = Set.map (\(Tuple id index) -> { id, index }) unconnectableInputs
  where
  unconnectableInputs = Set.filter (not <<< flip (canConnect output) state) allInputs

  allInputs =
    Set.fromFoldable
      $ ( \(Tuple id node) ->
            List.mapWithIndex (const <<< Tuple id) $ view _nodeInputs node
        )
      =<< nodeGroup

  nodeGroup :: List _
  nodeGroup = maybe mempty Map.toUnfoldable $ preview _currentNodes state

-- Generates a list of outputs which can't be connected
generateUnconnectableOutputs :: Tuple NodeId Int -> State -> Set.Set NodeId
generateUnconnectableOutputs input state = Set.filter (\outputId -> not $ canConnect outputId input state') outputs
  where
  state' = removeConnection input state

  keys = maybe mempty Map.keys $ preview _currentNodes state'

  outputNode = preview (_currentNodeGroup <<< _Just <<< _NodeGroupOutput) state'

  outputs = case outputNode of
    Just id -> Set.difference keys $ Set.singleton id
    Nothing -> keys

-- Generate the next expression / typeMap of a project
tryCompiling ::
  State ->
  { | CompilationResult () }
tryCompiling state@{ project } = result
  where
  expression' = compileProject project

  result =
    { typeErrors: errors
    , lintingErrors: lintingErrors
    , typeMap: typeMap
    , expression: inline $ dce expression'
    }
    where
    lintingErrors = lint expression'

    (Tuple typeMap (SolveState { errors })) = solveExpression expression'

-- Compile a project
compile :: State -> State
compile state =
  evaluate
    $ Record.merge overwrites state
  where
  overwrites = tryCompiling state

-- Evaluate the current expression and write into the value map
evaluate :: State -> State
evaluate state@{ runtimeOverwrites, expression } = state { valueMap = valueMap }
  where
  context =
    InterpreterContext
      { location: UnknownLocation
      , termEnv: mempty
      , overwrites: runtimeOverwrites
      , toplevel: true
      }

  valueMap =
    snd
      $ runInterpreter
          context
      $ interpret expression

-- Check if 2 pins can be connected
canConnect :: NodeId -> Tuple NodeId Int -> State -> Boolean
canConnect from (Tuple toId toIndex) state =
  fromMaybe false do
    let
      typeMap = view _typeMap state

      currentFunction = view _currentFunction state
    nodes <- preview _currentNodes state
    guard $ not $ G.wouldCreateCycle from toId $ toGraph nodes
    fromType <- getOutputType currentFunction from state
    toType <- Map.lookup (InsideFunction currentFunction $ PinLocation toId $ InputPin toIndex) typeMap
    guard $ canUnify toType fromType
    pure true

-- Creates a connection from a node id to a node id an an input index
createConnection :: NodeId -> NodeId -> Int -> State -> State
createConnection from toId toIndex =
  set
    ( _atCurrentNode toId
        <<< _nodeInput toIndex
    )
    (Just from)

-- Set the function the user is editing at the moment
setCurrentFunction :: FunctionName -> State -> State
setCurrentFunction = set _currentFunction

-- Creates a function, adds an output node and set it as the current edited function
initializeFunction :: forall m. MonadEffect m => FunctionName -> State -> m State
initializeFunction name state =
  flip execStateT state do
    let
      id = NodeId $ show name <> "-output"
    cache <- liftEffect emptyGeometryCache
    liftEffect $ Native.createNode cache id 1 false Nullable.null
    modify_ $ over _project $ createFunction name id
    modify_ $ setCurrentFunction name
    modify_ $ set _currentGeometryCache $ Just cache
    modify_ $ set (_atFunctionData name) $ Just $ internal [] { name: show name <> "-output", description: "The output of a custom functions" }
    modify_ compile

-- Remove a conenction from the current function
removeConnection :: Tuple NodeId Int -> State -> State
removeConnection (Tuple toId toIndex) =
  execState
    $ withCurrentFunction_ \currentFunction -> do
        modify_ $ set (_atNode currentFunction toId <<< _nodeInput toIndex) Nothing
        modify_ compile

-- Deletes a node form a given function
deleteNode :: FunctionName -> NodeId -> State -> State
deleteNode functionName id state =
  flip execState state
    $ when (not isOutput) do
        modify_
          $ over (_nodes functionName <<< traversed <<< _nodeInputs <<< traversed) \input -> if input == Just id then Nothing else input
        modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupInputs) $ filter (id /= _)
        modify_ $ over (_currentNodeGroup <<< _Just <<< _NodeGroupNodes)
          $ Map.filterKeys (id /= _)
        modify_ compile
  where
  node = preview (_atNode functionName id) state

  isOutput = maybe false (is _OutputNode) node

-- Delete all the nodes runnign a certain functions inside another functions
deleteFunctionReferences :: FunctionName -> FunctionName -> Map NodeId Node -> State -> State
deleteFunctionReferences toDelete functionName graph state =
  foldr (deleteNode functionName) state
    $ filterMap
        ( \(Tuple nodeId node) ->
            if getFunctionName node == toDelete then
              Just nodeId
            else
              Nothing
        )
    $ (Map.toUnfoldable graph :: List _)

-- Delete a function from the state
deleteFunction :: FunctionName -> State -> State
deleteFunction toDelete state =
  flip execState state
    $ unless (main == toDelete) do
        let
          visualFunctions =
            filterMap
              ( \(Tuple name function) ->
                  Tuple name
                    <$> preview _VisualFunction function
              )
              $ ( Map.toUnfoldable
                    $ view _functions state ::
                    List _
                )
        put
          $ foldr
              (\(Tuple functionName nodeGroup) -> deleteFunctionReferences toDelete functionName $ view _NodeGroupNodes nodeGroup)
              state
              visualFunctions
        modify_ $ set (_atFunctionData toDelete) Nothing
        modify_ $ set (_function toDelete) Nothing
        modify_ $ over (_runtimeOverwrites <<< _Newtype) $ Map.filterKeys $ (_ /= Just toDelete) <<< view _Function
        modify_ $ set (_atInputCount toDelete) Nothing
        modify_ $ set (_atGeometry toDelete) Nothing
        when (view _currentFunction state == toDelete) $ modify_ $ set _currentFunction main
        modify_ compile
  where
  main = view _ProjectMain state.project

-- Count the number of visual user-defined functions in a project
visualFunctionCount :: State -> Int
visualFunctionCount = Map.size <<< filter (is _VisualFunction) <<< view _functions

-- Get the number of nodes in a state
nodeCount :: State -> Int
nodeCount = unwrap <<< foldMap (Additive <<< go) <<< view _functions
  where
  go (VisualFunction (NodeGroup { nodes })) = length nodes

  go _ = 0

-- Search using the current search term in the state
searchNode :: State -> Array FunctionName
searchNode state = sortBySearch show searchTerm nodes
  where
  searchTerm = view _nodeSearchTerm state

  nodes = Set.toUnfoldable $ Map.keys $ view _functionData state

-- Check if a function exists
functionExists :: FunctionName -> State -> Boolean
functionExists name = Map.member name <<< view _functionData

-- Prevent and stop the propagation for any dom event
preventDefaults :: forall q i o m. MonadEffect m => Event -> HalogenM State q i o m Unit
preventDefaults event = liftEffect $ Event.preventDefault event *> Event.stopPropagation event

-- | Run an action which needs access to the current function
withCurrentFunction :: forall f r. MonadState State f => (FunctionName -> f r) -> f r
withCurrentFunction f = gets (view _currentFunction) >>= f

-- | Run an action which need access to the current function and which returns nothing
withCurrentFunction_ :: forall f r. MonadState State f => (FunctionName -> f r) -> f Unit
withCurrentFunction_ = void <<< withCurrentFunction

-- | Run an action which needs access to the current geometry cache
withCurrentGeometries :: forall f r. MonadState State f => (GeometryCache -> f r) -> f (Maybe r)
withCurrentGeometries f = gets (view _currentGeometryCache) >>= traverse f

-- | Run an action which needs access to the id of the node the user is curently editing
withCurrentNode_ :: forall f r. MonadState State f => (NodeId -> f r) -> f Unit
withCurrentNode_ f = void $ gets (_.currentlyEditedNode) >>= traverse f

-- | Get the maximum number of inputs a node can take
getMaxInputs ::
  forall r.
  FunctionName ->
  Record
    ( functionData :: Map.Map FunctionName FunctionData
    , project :: Project
    | r
    ) ->
  Int
getMaxInputs name { project: Project { functions }, functionData } = case Map.lookup name functionData of
  Just (FunctionData { inputs }) -> Array.length inputs
  Nothing -> case Map.lookup name functions of
    Just (VisualFunction (NodeGroup { inputs })) -> List.length inputs
    _ -> 0

-- | Get the value to be displayed underneath a node
getNodeValue :: FunctionName -> ValueMap Location -> NodeId -> Node -> Maybe (Term Location)
getNodeValue currentFunction (ValueMap vmap) id = case _ of
  ComplexNode _ -> Map.lookup (InsideFunction currentFunction $ NodeLocation id) vmap
  OutputNode _ -> Map.lookup (InsideFunction currentFunction $ PinLocation id $ InputPin 0) vmap
  _ -> Nothing

-- Update all nodes in the current geometry
updateAll :: forall m. MonadState State m => MonadEffect m => m (Maybe GeometryCache)
updateAll =
  withCurrentFunction \name -> do
    cache <- gets $ view $ _atGeometry name
    (map (maybe mempty Map.keys) $ gets $ preview $ _nodes name)
      >>= traverse_ updateNode
    pure cache

getFunctionColorMap :: Int -> Type -> ForeignTypeMap
getFunctionColorMap 0 ty = { inputs: [], output: Nullable.notNull $ cssStringRGBA $ typeToColor ty }

getFunctionColorMap n (TConstant "Function" [ from, to ]) =
  next
    { inputs =
      [ Nullable.notNull (cssStringRGBA $ typeToColor from) ]
        <> next.inputs
    }
  where
  next = getFunctionColorMap (n - 1) to

getFunctionColorMap n ty = getFunctionColorMap 0 ty

-- | Possible steps we need to take to focus on something
data MovementStep
  = MoveToFunction FunctionName
  | CenterNode NodeId
  | CenterOutput

-- | Get the steps for moving towards a location 
moveTo :: Location -> Array MovementStep
moveTo UnknownLocation = []

moveTo (AtFunction name) = [ MoveToFunction name ]

moveTo (AtFunctionDeclaration name) = [ MoveToFunction name ]

moveTo (FixpointOperator name) = [ MoveToFunction name ]

moveTo (InsideFunction name deep) = [ MoveToFunction name ] <> go deep
  where
  go (NodeLocation id) = [ CenterNode id ]

  go (NodeDefinition id) = [ CenterNode id ]

  go (PinLocation id _) = [ CenterNode id ]

  go (AtApplication id _) = [ CenterNode id ]

  go FunctionDeclaration = [ CenterOutput ]

  go _ = []

-- | Get the type to be displayed for a particular complex node
getNodeType :: NodeId -> FunctionName -> State -> Maybe Type
getNodeType id (FunctionName "if") { currentFunction, typeMap } = do
  output <- Map.lookup (InsideFunction currentFunction (NodeLocation id)) typeMap
  pure $ multiArgumentFuncion [ typeBool, output, output ] output

getNodeType id function { currentFunction, typeMap } =
  Map.lookup
    ( InsideFunction currentFunction
        ( FunctionUsage id function
        )
    )
    typeMap

-- Lenses
_inputCountMap :: Lens' State (Map FunctionName Int)
_inputCountMap = prop (SProxy :: _ "inputCountMap")

_atInputCount :: FunctionName -> Traversal' State (Maybe Int)
_atInputCount name = _inputCountMap <<< at name

_valueMap :: Lens' State (ValueMap Location)
_valueMap = prop (SProxy :: _ "valueMap")

_runtimeOverwrites :: Lens' State (ValueMap Location)
_runtimeOverwrites = prop (SProxy :: _ "runtimeOverwrites")

_geometries :: Lens' State (Map FunctionName GeometryCache)
_geometries = prop (SProxy :: _ "geometries")

_atGeometry :: FunctionName -> Lens' State (Maybe GeometryCache)
_atGeometry name = _geometries <<< at name

_functionData :: Lens' State (Map FunctionName FunctionData)
_functionData = prop (SProxy :: _ "functionData")

_atFunctionData :: FunctionName -> Traversal' State (Maybe FunctionData)
_atFunctionData name = _functionData <<< at name

_project :: Lens' State Project
_project = prop (SProxy :: _ "project")

_expression :: Lens' State (Expression Location)
_expression = prop (SProxy :: _ "expression")

_typeMap :: Lens' State (Map Location Type)
_typeMap = prop (SProxy :: _ "typeMap")

_nextId :: Lens' State Int
_nextId = prop (SProxy :: _ "nextId")

_functions :: Lens' State (Map.Map FunctionName DataflowFunction)
_functions = _project <<< _ProjectFunctions

_nodeGroup :: FunctionName -> Traversal' State NodeGroup
_nodeGroup name = _project <<< _projectNodeGroup name

_nodes :: FunctionName -> Traversal' State (Map NodeId Node)
_nodes name = _nodeGroup name <<< _NodeGroupNodes

_atNode :: FunctionName -> NodeId -> Traversal' State Node
_atNode name id = _project <<< _atProjectNode name id <<< _Just

_function :: FunctionName -> Traversal' State (Maybe DataflowFunction)
_function name = _project <<< _atProjectFunction name

_currentFunction :: Lens' State FunctionName
_currentFunction = prop (SProxy :: _ "currentFunction")

_panelIsOpen :: Lens' State Boolean
_panelIsOpen = prop (SProxy :: _ "panelIsOpen")

_currentTab :: Lens' State Tab
_currentTab = prop (SProxy :: _ "currentTab")

_currentNodeGroup :: Lens' State (Maybe NodeGroup)
_currentNodeGroup =
  ( lens
      ( \state ->
          let
            current = view _currentFunction state
          in
            preview (_nodeGroup current) state
      )
      ( \state maybeValue ->
          fromMaybe state do
            let
              currentFunction = view _currentFunction state
            value <- maybeValue
            pure $ set (_nodeGroup currentFunction) value state
      )
  )

_currentGeometryCache :: Lens' State (Maybe GeometryCache)
_currentGeometryCache =
  ( lens
      ( \state ->
          let
            currentFunction = view _currentFunction state
          in
            view (_atGeometry currentFunction) state
      )
      ( \state maybeValue ->
          let
            currentFunction = view _currentFunction state
          in
            set (_atGeometry currentFunction) maybeValue state
      )
  )

_currentNodes :: Traversal' State (Map NodeId Node)
_currentNodes = _currentNodeGroup <<< _Just <<< _NodeGroupNodes

_atCurrentNode :: NodeId -> Traversal' State Node
_atCurrentNode id = _currentNodes <<< ix id

_name :: Lens' State String
_name = prop (SProxy :: _ "name")

_isExample :: Lens' State Boolean
_isExample = prop (SProxy :: _ "isExample")

_isAdmin :: Lens' State Boolean
_isAdmin = prop (SProxy :: _ "isAdmin")

_isVisible :: Lens' State Boolean
_isVisible = prop (SProxy :: _ "isVisible")

_nodeSearchTerm :: Lens' State String
_nodeSearchTerm = prop (SProxy :: _ "nodeSearchTerm")
