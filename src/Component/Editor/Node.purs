module Lunarbox.Component.Editor.Node
  ( Input
  , SelectionStatus(..)
  , renderNode
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Default (def)
import Data.Int (toNumber)
import Data.Lens (set, view)
import Data.Lens.Index (ix)
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Halogen.HTML (HTML, IProp, ComponentHTML)
import Halogen.HTML.Events (onClick, onMouseDown)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..))
import Lunarbox.Capability.Editor.Node.Arc as Arc
import Lunarbox.Component.Editor.Edge (renderEdge)
import Lunarbox.Component.Editor.Node.Input (input)
import Lunarbox.Component.Editor.Node.Overlays (overlays)
import Lunarbox.Component.Editor.RuntimeValue (renderRuntimeValue)
import Lunarbox.Component.Utils (whenElem)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Editor.Constants (arcSpacing, arcWidth, inputLayerOffset, mouseId, nodeRadius, outputRadius, scaleConnectionPreview)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs, _FunctionDataOutput, _PinName)
import Lunarbox.Data.Editor.FunctionUi (FunctionUi)
import Lunarbox.Data.Editor.Node (Node(..), _nodeInput, _nodeInputs, getInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataPosition)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.NodeInput (getArcs)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Math (normalizeAngle)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Svg.Attributes (Linecap(..), strokeDashArray, strokeLinecap, strokeWidth, transparent)
import Lunarbox.Svg.Element (withLabel)
import Math (cos, floor, pi, sin)
import Svg.Attributes (Color)
import Svg.Attributes as SA
import Svg.Elements as SE
import Web.UIEvent.MouseEvent (MouseEvent)

-- A node can either have one of it's inputs, it's output or nothing selected
data SelectionStatus
  = InputSelected Int
  | OutputSelected
  | NothingSelected

type Input a s m
  = { nodeData :: NodeData
    , node :: Node
    , labels :: Array (ComponentHTML a s m)
    , functionData :: FunctionData
    , colorMap :: Map Pin SA.Color
    , hasOutput :: Boolean
    , nodeDataMap :: Map NodeId NodeData
    , selectionStatus :: SelectionStatus
    , mousePosition :: Vec2 Number
    , value :: Maybe RuntimeValue
    , ui :: Maybe (FunctionUi a s m)
    , unconnectablePins :: Set.Set Pin
    }

type Actions a
  = { select :: Maybe a
    , selectInput :: Int -> Maybe a
    , selectOutput :: Maybe a
    , setValue :: RuntimeValue -> Maybe a
    , removeConnection :: NodeId -> Int -> Maybe a
    }

output :: forall r a. Boolean -> String -> Maybe a -> Color -> HTML r a
output unconnectable label selectOutput color =
  withLabel label
    $ SE.circle
        [ SA.r 10.0
        , SA.fill $ Just color
        , SA.class_ $ "node-output" <> if unconnectable then " unconnectable" else ""
        , onClick $ const selectOutput
        ]

constant :: forall r a. HTML r a
constant =
  SE.circle
    [ SA.r nodeRadius
    , SA.fill $ Just transparent
    , SA.stroke $ Just $ SA.RGB 176 112 107
    , strokeWidth arcWidth
    , strokeLinecap Butt
    , strokeDashArray [ pi * nodeRadius / 20.0 ]
    ]

-- Helper to scale down the connectioon previews
scaleConnection :: NodeId -> Vec2 Number -> Vec2 Number
scaleConnection id position
  | id == mouseId = scaleConnectionPreview <$> position
  | otherwise = position

renderNode :: forall a s m. Input a s m -> Actions a -> ComponentHTML a s m
renderNode { nodeData: nodeData
, functionData
, labels
, colorMap
, hasOutput
, node
, nodeDataMap
, selectionStatus
, mousePosition
, value
, ui
, unconnectablePins
} { select
, selectOutput
, selectInput
, removeConnection
, setValue
} =
  SE.g
    [ SA.transform [ SA.Translate (centerPosition !! d0) (centerPosition !! d1) ]
    , SA.class_ "node"
    , allowMoving
    ]
    $ [ overlays maxRadius labels
      ]
    <> valueSvg
    <> arcs
    <> uiSvg
    <> [ movementHandler
      , whenElem hasOutput
          $ \_ ->
              output
                (OutputPin `Set.member` unconnectablePins)
                (view (_FunctionDataOutput <<< _PinName) functionData)
                selectOutput
                outputColor
      ]
    <> outputPartialEdge
  where
  movementHandler =
    SE.circle
      [ SA.r $ nodeRadius - arcWidth * 4.0
      , SA.fill $ Just transparent
      ]

  allowMoving :: forall r. IProp ( onMouseDown ∷ MouseEvent | r ) _
  allowMoving = onMouseDown $ const select

  outputColor =
    fromMaybe transparent
      $ Map.lookup OutputPin colorMap

  centerPosition = floor <$> view _NodeDataPosition nodeData

  (Tuple nodeDataWithMouse nodeWithMouse) = case selectionStatus of
    InputSelected index -> Tuple nodeDataMap' node'
      where
      node' = set (_nodeInput index) (Just mouseId) node

      nodeDataMap' = Map.insert mouseId (set _NodeDataPosition mousePosition def) nodeDataMap
    _ -> Tuple nodeDataMap node

  inputArcs = getArcs nodeDataWithMouse nodeData nodeWithMouse

  outputPartialEdge = case selectionStatus of
    OutputSelected ->
      pure
        $ renderEdge
            { from: zero
            , to: scaleConnectionPreview <$> (mousePosition - centerPosition)
            , color: outputColor
            , dotted: true
            , className: Nothing
            }
            { handleClick: Nothing
            }
    _ -> mempty

  maxRadius = nodeRadius + (toNumber $ List.length inputArcs - 1) * inputLayerOffset

  uiSvg =
    fromMaybe mempty do
      generateUi <- ui
      nodeValue <- value
      pure
        $ [ SE.g
              [ SA.transform
                  [ SA.Translate 0.0 $ maxRadius + inputLayerOffset * 3.0
                  ]
              ]
              [ generateUi { value: nodeValue } { setValue } ]
          ]

  valueSvg =
    maybe mempty
      ( \value' ->
          let
            color = case node of
              OutputNode _ -> fromMaybe transparent $ Map.lookup (InputPin 0) colorMap
              _ -> outputColor
          in
            pure
              $ SE.g
                  [ SA.transform
                      [ SA.Translate 0.0 $ maxRadius + inputLayerOffset
                      ]
                  ]
                  [ renderRuntimeValue color value'
                  ]
      )
      value

  arcs =
    if List.null $ view _nodeInputs node then
      [ constant ]
    else
      inputArcs
        # List.mapWithIndex
            ( \layer inputsLayer ->
                inputsLayer
                  >>= \arc@(Arc start end index) ->
                      let
                        -- The middle of the arc
                        angle = normalizeAngle $ start + Arc.length arc / 2.0

                        -- The radius of the arc
                        radius = nodeRadius + (toNumber layer) * inputLayerOffset

                        -- Position of the middle of this arc
                        inputPosition = (_ * radius) <$> vec2 (cos angle) (sin angle)

                        -- The color of the input arc
                        inputColor = fromMaybe transparent $ Map.lookup (InputPin index) colorMap

                        -- If this is true we make the user know he cannect this pin
                        unconnectable = InputPin index `Set.member` unconnectablePins

                        -- The edge to render
                        edge =
                          maybe mempty pure do
                            nodeId <- join $ getInputs nodeWithMouse `List.index` index
                            targetData <- Map.lookup nodeId nodeDataWithMouse
                            color <- Map.lookup (InputPin index) colorMap
                            let
                              targetPosition = view _NodeDataPosition targetData

                              isMouse = nodeId == mouseId

                              originalPosition = scaleConnection nodeId $ targetPosition - centerPosition
                            pure
                              $ renderEdge
                                  { from: inputPosition
                                  , to:
                                    if isMouse then
                                      originalPosition
                                    else
                                      originalPosition - ((_ * outputRadius) <$> vec2 (cos angle) (sin angle))
                                  , color
                                  , dotted: isMouse
                                  , className: Just "node-connection"
                                  }
                                  { handleClick: guard (not isMouse) >>= const (removeConnection nodeId index)
                                  }

                        inputSvg =
                          input
                            { arc
                            , spacing:
                              if List.length inputsLayer == 1 then
                                0.0
                              else
                                arcSpacing
                            , radius
                            , color: inputColor
                            , unconnectable
                            , tooltip: view (_FunctionDataInputs <<< ix index <<< _PinName) functionData
                            }
                            $ selectInput index
                      in
                        inputSvg : edge
            )
        # join
        # List.toUnfoldable
