module Lunarbox.Component.Editor.Add
  ( add
  ) where

import Prelude
import Data.Default (def)
import Data.Int (fromString, toNumber)
import Data.List ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (replicate)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Editor.Type (generateColorMap, prettify)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Editor.Node (SelectionStatus(..), renderNode)
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container, whenElem)
import Lunarbox.Data.Dataflow.Type (Type, inputs, output)
import Lunarbox.Data.Editor.Constants (arcWidth, nodeRadius)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.Node (Node(..), hasOutput)
import Lunarbox.Data.Editor.Node.NodeDescriptor (NodeDescriptor, describe)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (Project)
import Lunarbox.Data.Editor.State (getMaxInputs)
import Lunarbox.Data.Ord (sortBySearch)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { project :: Project
    , currentFunction :: Maybe FunctionName
    , functionData :: Map.Map FunctionName FunctionData
    , typeMap :: Map.Map Location Type
    , inputCountMap :: Map.Map FunctionName Int
    , nodeSearchTerm :: String
    }

type Actions a
  = { edit :: FunctionName -> Maybe a
    , delete :: FunctionName -> Maybe a
    , addNode :: FunctionName -> Maybe a
    , changeInputCount :: FunctionName -> Int -> Maybe a
    }

resolvePin :: Pin -> Type -> Maybe Type
resolvePin (InputPin index) type' = inputs type' !! index

resolvePin OutputPin type' = Just $ output type'

nodeInput :: forall a s m. Int -> Map.Map Location Type -> FunctionName -> FunctionData -> NodeC.Input a s m
nodeInput inputCount typeMap name functionData =
  { nodeData: def
  , node
  , functionData
  , labels: mempty
  , unconnectablePins: mempty
  , nodeDataMap: mempty
  , hasOutput: hasOutput node
  , selectionStatus: NothingSelected
  , mousePosition: zero
  , colorMap:
    generateColorMap
      (\pin -> Map.lookup (AtFunction name) typeMap >>= resolvePin pin)
      node
  , value: Nothing
  , ui: Nothing
  }
  where
  node =
    ComplexNode
      { inputs: replicate inputCount Nothing
      , function: name
      }

-- The little icon buttons next to each node
nodeButton :: forall a s m. Boolean -> Maybe a -> String -> HH.ComponentHTML a s m
nodeButton active handleClick iconName =
  whenElem active \_ ->
    HH.div
      [ onClick $ const handleClick
      ]
      [ icon iconName ]

makeNode ::
  forall a s m.
  Actions a ->
  NodeDescriptor ->
  FunctionName ->
  Int ->
  Map.Map Location Type ->
  Map.Map FunctionName Int -> FunctionData -> HH.ComponentHTML a s m
makeNode { edit, addNode, changeInputCount, delete } { isUsable, isEditable, canBeDeleted } name maxInputs typeMap inputCountMap functionData =
  HH.div [ className "node" ]
    [ SE.svg
        [ SA.width 75.0
        , SA.height 75.0
        , let size = arcWidth + nodeRadius in SA.viewBox (-size) (-size) (2.0 * size) (2.0 * size)
        ]
        [ renderNode
            (nodeInput inputCount typeMap name functionData)
            { select: const Nothing
            , selectOutput: const Nothing
            , selectInput: const $ const Nothing
            , setValue: const Nothing
            , removeConnection: const $ const $ const Nothing
            }
        ]
    , container "node-data"
        [ container "node-text"
            [ container "node-header"
                [ HH.div [ HP.id_ "node-name", className "no-overflow" ]
                    [ HH.text $ show name
                    ]
                , nodeButton isUsable (addNode name) "add"
                , nodeButton isEditable (edit name) "edit"
                , nodeButton canBeDeleted (delete name) "delete"
                ]
            , container "node-type"
                $ fromMaybe mempty
                $ pure
                <<< highlightTypeToHTML
                <<< prettify
                <$> Map.lookup (AtFunction name) typeMap
            , container "curry-node"
                [ container "curry-text" [ HH.text "inputs:" ]
                , HH.input
                    [ HP.value $ show inputCount
                    , HP.type_ $ HP.InputNumber
                    , HP.min 0.0
                    , HP.max $ toNumber maxInputs
                    , onValueInput $ changeInputCount name <=< map (clamp 0 maxInputs) <<< fromString
                    ]
                ]
            ]
        ]
    ]
  where
  inputCount = fromMaybe maxInputs $ Map.lookup name inputCountMap

add :: forall a s m. Input -> Actions a -> HH.ComponentHTML a s m
add input@{ project, currentFunction, functionData, typeMap, inputCountMap, nodeSearchTerm } actions =
  container "nodes"
    $ ( \(Tuple name descriptor) ->
          let
            functionData' = fromMaybe def $ Map.lookup name functionData
          in
            makeNode actions descriptor name (getMaxInputs name input) typeMap inputCountMap functionData'
      )
    <$> sortBySearch (show <<< fst) nodeSearchTerm
        ( Map.toUnfoldable
            $ describe currentFunction project
        )
