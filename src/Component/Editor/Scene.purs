module Lunarbox.Component.Editor.Scene
  ( Input
  , Actions
  , scene
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (sortBy)
import Data.Bifunctor (bimap)
import Data.Default (def)
import Data.Either (Either, either, note)
import Data.Int (toNumber)
import Data.Lens (is, preview, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Halogen.HTML (ComponentHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp)
import Lunarbox.Capability.Editor.Type (ColoringError, generateTypeMap, prettify)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToSvg)
import Lunarbox.Component.Editor.Node (renderNode)
import Lunarbox.Component.Editor.Node.Label (labelText, label)
import Lunarbox.Data.Dataflow.Expression (Expression, sumarizeExpression)
import Lunarbox.Data.Dataflow.Expression as Expression
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Runtime.ValueMap (ValueMap)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.Camera (Camera, toViewBox, toWorldCoordinates)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), _ExtendedLocation, _LocationExtension)
import Lunarbox.Data.Editor.FunctionData (FunctionData, getFunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataPosition)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.PartialConnection (PartialConnection, getSelectionStatus)
import Lunarbox.Data.Editor.Project (Project, _atProjectNode)
import Lunarbox.Data.Map (maybeBimap)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Page.Editor.EmptyEditor (erroredEditor)
import Svg.Attributes (Color(..))
import Svg.Attributes as SA
import Svg.Elements as SE
import Unsafe.Coerce (unsafeCoerce)
import Web.UIEvent.MouseEvent as ME

type Input a s m
  = { project :: Project
    , functionName :: FunctionName
    , typeMap :: Map Location Type
    , expression :: Expression Location
    , typeColors :: Map Location Color
    , functionData :: Map FunctionName FunctionData
    , nodeData :: Map NodeId NodeData
    , partialConnection :: PartialConnection
    , lastMousePosition :: Maybe (Vec2 Number)
    , valueMap :: ValueMap Location
    , functionUis :: Map FunctionName (FunctionUi a s m)
    , camera :: Camera
    , scale :: Vec2 Number
    }

type Actions a
  = { mouseMove :: Int -> Vec2 Number -> Maybe a
    , mouseDown :: Vec2 Number -> Maybe a
    , selectNode :: NodeId -> Maybe a
    , mouseUp :: Maybe a
    , selectInput :: NodeId -> Int -> Maybe a
    , selectOutput :: NodeId -> Maybe a
    , removeConnection :: NodeId -> Tuple NodeId Int -> Maybe a
    , setValue :: FunctionName -> NodeId -> RuntimeValue -> Maybe a
    }

-- Errors which could arise while creating the node svg
data NodeBuildingError
  = MissingFunctionData FunctionName
  | MissingExpression NodeId
  | MissingNode NodeId
  | MissingType Location
  | LiftedError ColoringError

instance showNodeBuildingError :: Show NodeBuildingError where
  show = case _ of
    MissingFunctionData name -> "Cannot find function data for function " <> show name
    MissingExpression id -> "Cannot find compiliation result for node " <> show id
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
, expression
, functionData
, typeColors
, partialConnection
, lastMousePosition
, nodeData: nodeDataMap
, valueMap
, functionUis
} { selectNode, selectInput, selectOutput, removeConnection, setValue } (Tuple id nodeData) = do
  let
    generateLocation = DeepLocation functionName

    position = view _NodeDataPosition nodeData

    location = generateLocation $ Location id
  node <- getNode functionName id project
  nodeType <- note (MissingType location) $ Map.lookup location typeMap
  nodeExpression <- note (MissingExpression id) $ Expression.lookup location expression
  let
    name = getNodeName node

    localTypeMap =
      flip maybeBimap typeMap \location' type' -> do
        locationName <- preview _ExtendedLocation location'
        locationId <- preview (_LocationExtension <<< _ExtendedLocation) location'
        locationPin <- preview (_LocationExtension <<< _LocationExtension) location'
        guard $ locationName == functionName
        guard $ locationId == id
        pure $ Tuple locationPin type'

    nodeFunctionData = getFunctionData (\name' -> fromMaybe def $ Map.lookup name' functionData) node
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
          [ labelText $ show name
          , label $ highlightTypeToSvg (RGB 255 255 255) $ prettify nodeType
          , labelText $ sumarizeExpression nodeExpression
          ]
        , hasOutput: not $ is _OutputNode node
        , selectionStatus: getSelectionStatus partialConnection id
        , mousePosition: fromMaybe zero lastMousePosition
        , value: Map.lookup location $ unwrap valueMap
        , ui: unsafeCoerce $ Map.lookup name functionUis
        }
        { select: selectNode id
        , selectInput: selectInput id
        , selectOutput: selectOutput id
        , removeConnection: (_ <<< Tuple id) <<< removeConnection
        , setValue: setValue functionName id
        }

scene :: forall a s m. Input a s m -> Actions a -> ComponentHTML a s m
scene state@{ nodeData, camera, scale, lastMousePosition
} actions@{ mouseMove, mouseDown, mouseUp, selectNode } = either (\err -> erroredEditor $ show err) success nodeHtml
  where
  sortedNodes :: Array (Tuple NodeId NodeData)
  sortedNodes =
    sortBy (\(Tuple _ v) (Tuple _ v') -> compare v v')
      $ Map.toUnfoldable nodeData

  state' = state { lastMousePosition = toWorldCoordinates camera <$> lastMousePosition }

  nodeHtml = sequence $ (createNodeComponent state' actions <$> sortedNodes :: Array (Either _ _))

  success =
    SE.svg
      [ SA.width $ scale !! d0
      , SA.height $ scale !! d1
      , SA.id "scene"
      , toViewBox scale camera
      , onMouseMove $ \e -> mouseMove (ME.buttons e) $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseDown $ \e -> mouseDown $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseUp $ const mouseUp
      ]
