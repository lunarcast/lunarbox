module Lunarbox.Component.Editor.Scene
  ( scene
  , Input
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (sortBy)
import Data.Bifunctor (bimap)
import Data.Default (def)
import Data.Either (Either, either, note)
import Data.Int (toNumber)
import Data.Lens (is, preview)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Vec (vec2)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseUp)
import Lunarbox.Capability.Editor.Type (ColoringError, generateTypeMap, prettify)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToSvg)
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Component.Editor.Node.Label (labelText, label)
import Lunarbox.Data.Dataflow.Expression (Expression, sumarizeExpression)
import Lunarbox.Data.Dataflow.Expression as Expression
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..), _ExtendedLocation, _LocationExtension)
import Lunarbox.Data.Editor.FunctionData (FunctionData, getFunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), _OutputNode)
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..))
import Lunarbox.Data.Editor.Project (Project, _atProjectNode)
import Lunarbox.Data.Map (maybeBimap)
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
    , selectInput :: Tuple NodeId Int -> Maybe a
    , selectOutput :: NodeId -> Maybe a
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

createNodeComponent :: forall h a. Input -> Actions a -> Tuple NodeId NodeData -> NodeBuild (HH.HTML h a)
createNodeComponent { functionName
, project
, typeMap
, expression
, functionData
, typeColors
} { selectNode, selectInput, selectOutput } (Tuple id nodeData) = do
  let
    generateLocation = DeepLocation functionName

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
    $ NodeC.node
        { node
        , nodeData
        , functionData: nodeFunctionData
        , colorMap
        , labels:
          [ labelText $ show name
          , label $ highlightTypeToSvg (RGB 255 255 255) $ prettify nodeType
          , labelText $ sumarizeExpression nodeExpression
          ]
        , hasOutput: not $ is _OutputNode node
        }
        { select: selectNode id
        , selectInput: selectInput <<< Tuple id
        , selectOutput: selectOutput id
        }

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

  success =
    SE.svg
      [ SA.width 100000.0
      , SA.height 100000.0
      , onMouseMove $ \e -> mouseMove $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseDown $ \e -> mouseDown $ toNumber <$> vec2 (ME.pageX e) (ME.pageY e)
      , onMouseUp $ const mouseUp
      ]
