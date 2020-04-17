module Lunarbox.Component.Editor.Scene
  ( scene
  , Input
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (sortBy)
import Data.Array as Array
import Data.Default (def)
import Data.Either (Either, either, note)
import Data.Int (toNumber)
import Data.Lens (is, preview, view)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Vec (vec2)
import Debug.Trace (trace)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp)
import Lunarbox.Capability.Editor.Type (typeToColor)
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Data.Dataflow.Expression (Expression, sumarizeExpression)
import Lunarbox.Data.Dataflow.Expression as Expression
import Lunarbox.Data.Dataflow.Type (Type(..))
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs, getFunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode, hasOutput)
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..))
import Lunarbox.Data.Editor.Project (Project, _atProjectNode)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (erroredEditor)
import Svg.Attributes (Color(..))
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.UIEvent.MouseEvent as ME

type Input
  = { project :: Project
    , functionName :: FunctionName
    , nodeGroup :: NodeGroup
    , typeMap :: Map Location Type
    , expression :: Expression Location
    , lastMousePosition :: Maybe (Vec2 Number)
    , typeColors :: Map Location Color
    , functionData :: Map FunctionName FunctionData
    , nodeData :: Map NodeId NodeData
    }

type Actions a
  = { mouseMove :: Vec2 Number -> Maybe a
    , mouseDown :: Vec2 Number -> Maybe a
    , selectNode :: NodeId -> Maybe a
    , mouseUp :: Maybe a
    }

-- Errors which could arise while creating the node svg
data NodeBuildingError
  = MissingFunctionData FunctionName
  | MissingExpression NodeId
  | MissingNode NodeId
  | MissingType Location
  | MissingColor Location
  | UnableToColor Type Location

instance showNodeBuildingError :: Show NodeBuildingError where
  show = case _ of
    MissingFunctionData name -> "Cannot find function data for function " <> show name
    MissingExpression id -> "Cannot find compiliation result for node " <> show id
    MissingNode id -> "Cannot find node " <> show id
    MissingType location -> "Cannot find inferred type for " <> show location
    MissingColor location -> "Cannot find color for " <> show location
    UnableToColor type' location -> "Unable to encode type " <> show type' <> " into a color at " <> show location

type NodeBuild
  = Either NodeBuildingError

-- Get a shade of gray based on a cusom int generator
greyShade :: (Int -> Int -> Int) -> Color
greyShade generate = let c = generate 100 255 in RGB c c c

getNodeName :: Node -> FunctionName
getNodeName = case _ of
  ComplexNode { function } -> function
  InputNode -> FunctionName "input"
  OutputNode _ -> FunctionName "output"

getNode :: FunctionName -> NodeId -> Project -> NodeBuild Node
getNode name id = note (MissingNode id) <<< join <<< (preview $ _atProjectNode name id)

createNodeComponent :: forall h a. Input -> Actions a -> Tuple NodeId NodeData -> NodeBuild (HH.HTML h a)
createNodeComponent { functionName, project, typeMap, expression, functionData, typeColors } { selectNode } (Tuple id nodeData) = do
  let
    generateLocation = DeepLocation functionName

    location = generateLocation $ Location id
  node <- getNode functionName id project
  nodeType <- note (MissingType location) $ Map.lookup location typeMap
  nodeExpression <- note (MissingExpression id) $ Expression.lookup location expression
  let
    name = getNodeName node

    nodeFunctionData = getFunctionData (\name' -> fromMaybe def $ Map.lookup name' functionData) node

    inputPints = List.mapWithIndex (\index _ -> InputPin index) $ Array.toUnfoldable $ view _FunctionDataInputs nodeFunctionData

    pinLocations = (OutputPin <$ guard (hasOutput node)) <> inputPints

    toColor currentLocation = do
      let
        fullLocation = generateLocation $ DeepLocation id currentLocation
      pinType <- note (MissingType fullLocation) $ Map.lookup fullLocation typeMap
      color <- case pinType of
        TVarariable name' -> note (MissingColor fullLocation) $ Map.lookup fullLocation typeColors
        other -> note (UnableToColor other fullLocation) $ typeToColor other
      pure $ Tuple currentLocation color
  colorMap <- Map.fromFoldable <$> (sequence $ toColor <$> pinLocations)
  let
    a = trace colorMap \_ -> "hi"
  pure
    $ NodeC.node
        { node
        , nodeData
        , functionData: nodeFunctionData
        , colorMap
        , labels: [ show name, show nodeType, sumarizeExpression nodeExpression ]
        , hasOutput: not $ is _OutputNode node
        }
        { select: selectNode id }

scene :: forall h a. Input -> Actions a -> HH.HTML h a
scene state@{ project
, expression
, typeMap
, typeColors
, functionName
, nodeData
, functionData
, nodeGroup: (NodeGroup { nodes })
} actions@{ mouseMove, mouseDown, mouseUp, selectNode } = either (\err -> erroredEditor $ show err) success nodeHtml
  where
  sortedNodes :: Array (Tuple NodeId NodeData)
  sortedNodes =
    sortBy (\(Tuple _ v) (Tuple _ v') -> compare v v')
      $ Map.toUnfoldable nodeData

  nodeHtml = sequence $ (createNodeComponent state actions <$> sortedNodes :: Array (Either _ _))

  a = trace sortedNodes \_ -> "hi"

  success =
    SE.svg
      [ SA.width 100000.0
      , SA.height 100000.0
      , onMouseMove $ \e -> mouseMove $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseDown $ \e -> mouseDown $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseUp $ const mouseUp
      ]
