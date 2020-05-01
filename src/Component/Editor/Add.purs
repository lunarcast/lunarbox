module Lunarbox.Component.Editor.Add
  ( add
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Default (def)
import Data.Either (either)
import Data.Int (fromString, toNumber)
import Data.List ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Editor.Type (generateTypeMap, prettify)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Editor.Node (SelectionStatus(..), renderNode)
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Data.Dataflow.Type (Type, inputs, numberOfInputs, output)
import Lunarbox.Data.Editor.Constants (arcWidth, nodeRadius)
import Lunarbox.Data.Editor.ExtendedLocation (ExtendedLocation(..))
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node(..), hasOutput)
import Lunarbox.Data.Editor.Node.NodeDescriptor (NodeDescriptor, describe)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (Project)
import Svg.Attributes (Color(..))
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { project :: Project
    , currentFunction :: Maybe FunctionName
    , functionData :: Map.Map FunctionName FunctionData
    , typeMap :: Map.Map Location Type
    , inputCountMap :: Map.Map FunctionName Int
    }

type Actions a
  = { edit :: FunctionName -> Maybe a
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
  , hasOutput: hasOutput node
  , nodeDataMap: mempty
  , selectionStatus: NothingSelected
  , mousePosition: zero
  , colorMap:
    either (const mempty) identity
      $ generateTypeMap
          (\pin -> Map.lookup (Location name) typeMap >>= resolvePin pin)
          functionData
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

makeNode ::
  forall a s m.
  Actions a ->
  NodeDescriptor ->
  FunctionName ->
  Map.Map Location Type ->
  Map.Map FunctionName Int -> FunctionData -> HH.ComponentHTML a s m
makeNode { edit, addNode, changeInputCount } { isUsable, isEditable } name typeMap inputCountMap functionData =
  HH.div [ className "node" ]
    [ SE.svg
        [ SA.width 75.0
        , SA.height 75.0
        , let size = arcWidth + nodeRadius in SA.viewBox (-size) (-size) (2.0 * size) (2.0 * size)
        ]
        [ renderNode
            (nodeInput inputCount typeMap name functionData)
            { select: Nothing
            , selectOutput: Nothing
            , selectInput: const Nothing
            , setValue: const Nothing
            , removeConnection: const $ const Nothing
            }
        ]
    , container "node-data"
        [ container "node-text"
            [ HH.div [ HP.id_ "node-name", className "no-overflow" ]
                [ HH.text $ show name
                ]
            , container "node-type"
                $ fromMaybe mempty
                $ pure
                <<< highlightTypeToHTML (RGB 255 255 255)
                <<< prettify
                <$> Map.lookup (Location name) typeMap
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
        , container "node-buttons"
            [ HH.div
                [ HP.classes $ ClassName <$> ("active" <$ guard isUsable)
                , onClick $ const if isUsable then addNode name else Nothing
                ]
                [ icon "add" ]
            , HH.div
                [ HP.classes $ ClassName <$> ("active" <$ guard isEditable)
                , onClick $ const if isEditable then edit name else Nothing
                ]
                [ icon "edit" ]
            ]
        ]
    ]
  where
  maxInputs = fromMaybe 0 $ numberOfInputs <$> Map.lookup (Location name) typeMap

  inputCount = fromMaybe maxInputs $ Map.lookup name inputCountMap

add :: forall a s m. Input -> Actions a -> HH.ComponentHTML a s m
add { project, currentFunction, functionData, typeMap, inputCountMap } actions =
  container "nodes"
    $ ( \(Tuple name descriptor) ->
          let
            functionData' = fromMaybe def $ Map.lookup name functionData
          in
            makeNode actions descriptor name typeMap inputCountMap functionData'
      )
    <$> ( Map.toUnfoldable
          $ describe currentFunction project
      )
