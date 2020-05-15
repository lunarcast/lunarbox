module Lunarbox.Component.Editor.Scene
  ( Input
  , Actions
  , scene
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (foldr, sortBy)
import Data.Bifunctor (bimap)
import Data.Default (def)
import Data.Either (Either(..), either, note)
import Data.Lens (is, preview, view)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord (signum)
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (ComponentHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp, onWheel)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Editor.Type (ColoringError, generateTypeMap, prettify)
import Lunarbox.Component.Editor.Comment (comment)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToSvg)
import Lunarbox.Component.Editor.Node (renderNode)
import Lunarbox.Component.Editor.Node.Label (labelText, label)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Dataflow.Type (Type, inputs, typeFunction, typeString)
import Lunarbox.Data.Editor.Camera (Camera, toViewBox, toWorldCoordinates)
import Lunarbox.Data.Editor.Constants (scrollStep)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), _ExtendedLocation, _LocationExtension)
import Lunarbox.Data.Editor.FunctionData (FunctionData, getFunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _ComplexNode, _OutputNode, _nodeInputs)
import Lunarbox.Data.Editor.Node.CommentData (CommentData(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataComment, _NodeDataPosition)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (_NodeGroupInputs)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, getSelectionStatus)
import Lunarbox.Data.Editor.Project (Project, _atProjectNode, _projectNodeGroup)
import Lunarbox.Data.Editor.State (sceneRef)
import Lunarbox.Data.Map (maybeBimap)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (erroredEditor)
import Math (pow)
import Svg.Attributes (Color(..))
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (deltaY)

type Input a s m
  = { project :: Project
    , functionName :: FunctionName
    , typeMap :: Map Location Type
    , typeColors :: Map Location Color
    , functionData :: Map FunctionName FunctionData
    , nodeData :: Map NodeId NodeData
    , partialConnection :: PartialConnection
    , lastMousePosition :: Vec2 Number
    , valueMap :: ValueMap Location
    , functionUis :: Map FunctionName (FunctionUi a s m)
    , camera :: Camera
    , scale :: Vec2 Number
    , unconnectablePins :: Set.Set (ExtendedLocation NodeId Pin)
    }

type Actions a
  = { mouseMove :: MouseEvent -> Maybe a
    , mouseDown :: MouseEvent -> Maybe a
    , mouseUp :: Maybe a
    , selectNode :: NodeId -> Event -> Maybe a
    , zoom :: Number -> Maybe a
    , stopPropagation :: Event -> Maybe a
    , selectInput :: NodeId -> Int -> Event -> Maybe a
    , selectOutput :: NodeId -> Event -> Maybe a
    , removeConnection :: NodeId -> Tuple NodeId Int -> Event -> Maybe a
    , setValue :: FunctionName -> NodeId -> RuntimeValue -> Maybe a
    , handleCommentChange :: NodeId -> Event -> Maybe a
    }

-- Errors which could arise while creating the node svg
data NodeBuildingError
  = MissingFunctionData FunctionName
  | MissingNode NodeId
  | MissingType Location
  | LiftedError ColoringError

instance showNodeBuildingError :: Show NodeBuildingError where
  show = case _ of
    MissingFunctionData name -> "Cannot find function data for function " <> show name
    MissingNode id -> "Cannot find node " <> show id
    MissingType location -> "Cannot find inferred type for " <> show location
    LiftedError error -> show error

type NodeBuild
  = Either NodeBuildingError

getNodeName :: Node -> FunctionName
getNodeName = case _ of
  ComplexNode { function } -> function
  InputNode -> FunctionName "input"
  OutputNode _ -> FunctionName "output"

getNode :: FunctionName -> NodeId -> Project -> NodeBuild Node
getNode name id = note (MissingNode id) <<< join <<< (preview $ _atProjectNode name id)

createNodeComponent :: forall s a m. Input a s m -> Actions a -> Tuple NodeId NodeData -> NodeBuild (ComponentHTML a s m)
createNodeComponent { functionName
, project
, typeMap
, functionData
, typeColors
, partialConnection
, lastMousePosition
, nodeData: nodeDataMap
, valueMap
, functionUis
, unconnectablePins
} { selectNode, selectInput, selectOutput, removeConnection, setValue } (Tuple id nodeData) = do
  let
    generateLocation = DeepLocation functionName

    position = view _NodeDataPosition nodeData

    currentFunctionLocation = Location functionName

    location = generateLocation $ Location id
  nodeGroup <- note (MissingType location) $ preview (_projectNodeGroup functionName) project
  let
    inputIndex = List.findIndex (_ == id) $ view _NodeGroupInputs nodeGroup
  node <- getNode functionName id project
  currentFunctionType <- note (MissingType currentFunctionLocation) $ Map.lookup currentFunctionLocation typeMap
  nodeType <- case inputIndex of
    Just index -> note (MissingType location) $ (inputs currentFunctionType) `List.index` index
    Nothing -> note (MissingType location) $ Map.lookup location typeMap
  let
    name = getNodeName node

    localTypeMap = case inputIndex of
      Just index -> Map.singleton OutputPin nodeType
      Nothing ->
        flip maybeBimap typeMap \location' type' -> do
          locationName <- preview _ExtendedLocation location'
          locationId <- preview (_LocationExtension <<< _ExtendedLocation) location'
          locationPin <- preview (_LocationExtension <<< _LocationExtension) location'
          guard $ locationName == functionName
          guard $ locationId == id
          pure $ Tuple locationPin type'

    localUnconnectablePins =
      flip Set.mapMaybe unconnectablePins \location' -> do
        locationId <- preview _ExtendedLocation location'
        guard $ locationId == id
        preview _LocationExtension location'

    nodeFunctionData = getFunctionData (\name' -> fromMaybe def $ Map.lookup name' functionData) node
  functionType <-
    if is _ComplexNode node then
      note (MissingType $ Location name) $ Map.lookup (Location name) typeMap
    else
      pure $ typeString
  colorMap <- bimap LiftedError identity $ generateTypeMap (flip Map.lookup localTypeMap) nodeFunctionData node
  pure
    $ HH.lazy2
        renderNode
        { node
        , nodeData
        , functionData: nodeFunctionData
        , colorMap
        , nodeDataMap
        , labels:
          [ label $ highlightTypeToSvg (RGB 255 255 255) $ prettify
              $ foldr typeFunction nodeType
              $ (List.mapWithIndex \index type' -> fromMaybe type' $ Map.lookup (InputPin index) localTypeMap)
              $ List.take (List.length $ view _nodeInputs node)
              $ inputs functionType
          , labelText $ show name
          ]
        , hasOutput: not $ is _OutputNode node
        , selectionStatus: getSelectionStatus partialConnection id
        , mousePosition: lastMousePosition
        , value: Map.lookup location $ unwrap valueMap
        , ui: Map.lookup name functionUis
        , unconnectablePins: localUnconnectablePins
        }
        { select: selectNode id
        , selectInput: selectInput id
        , selectOutput: selectOutput id
        , removeConnection: (_ <<< Tuple id) <<< removeConnection
        , setValue: setValue functionName id
        }

scene :: forall a s m. Actions a -> Input a s m -> ComponentHTML a s m
scene actions@{ mouseMove, mouseUp, mouseDown, selectNode, zoom, stopPropagation, handleCommentChange } state@{ nodeData, camera, scale, lastMousePosition } =
  either
    (\err -> erroredEditor $ show err)
    success
    nodeHtml
  where
  sortedNodes :: Array (Tuple NodeId NodeData)
  sortedNodes =
    sortBy (\(Tuple _ v) (Tuple _ v') -> compare v v')
      $ Map.toUnfoldable nodeData

  state' = state { lastMousePosition = toWorldCoordinates camera lastMousePosition }

  nodeHtml =
    sequence
      $ ( \nodeDataWithId@(Tuple id currentNodeData) -> case view _NodeDataComment currentNodeData of
            Just (CommentData { text, scale: commentScale }) ->
              Right
                $ comment
                    { text
                    , scale: commentScale
                    , position: view _NodeDataPosition currentNodeData
                    }
                    { select: selectNode id
                    , change: handleCommentChange id
                    , stopPropagation
                    }
            Nothing -> createNodeComponent state' actions nodeDataWithId
        )
      <$> sortedNodes

  success =
    HH.div [ HP.ref sceneRef ]
      <<< pure
      <<< SE.svg
          [ SA.width $ scale !! d0
          , SA.height $ scale !! d1
          , SA.id "scene"
          , toViewBox scale camera
          , onMouseMove mouseMove
          , onMouseDown mouseDown
          , onWheel \e -> zoom $ pow scrollStep $ signum $ deltaY e
          , onMouseUp $ const mouseUp
          ]
