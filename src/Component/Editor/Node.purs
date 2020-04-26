module Lunarbox.Component.Editor.Node
  ( Input
  , SelectionStatus(..)
  , renderNode
  ) where

import Prelude
import Data.Int (toNumber)
import Data.Lens (view)
import Data.List ((:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onMouseDown)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..))
import Lunarbox.Capability.Editor.Node.Arc as Arc
import Lunarbox.Component.Editor.Edge (renderEdge)
import Lunarbox.Component.Editor.Node.Input (input)
import Lunarbox.Component.Editor.Node.Overlays (overlays)
import Lunarbox.Data.Editor.Constants (arcSpacing, arcWidth, inputLayerOffset, nodeRadius)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.Node (Node, _nodeInputs, getInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataPosition)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.NodeInput (getArcs)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Math (normalizeAngle)
import Lunarbox.Svg.Attributes (Linecap(..), strokeDashArray, strokeLinecap, strokeWidth, transparent)
import Math (cos, pi, sin)
import Svg.Attributes (Color)
import Svg.Attributes as SA
import Svg.Elements as SE

-- A node can either have one of it's inputs, it's output or nothing selected
data SelectionStatus
  = InputSelected Int
  | OutputSelected
  | NothingSelected

type Input h a
  = { nodeData :: NodeData
    , node :: Node
    , labels :: Array (HTML h a)
    , functionData :: FunctionData
    , colorMap :: Map Pin SA.Color
    , hasOutput :: Boolean
    , nodeDataMap :: Map NodeId NodeData
    , selectionStatus :: SelectionStatus
    }

type Actions a
  = { select :: Maybe a
    , selectInput :: Int -> Maybe a
    , selectOutput :: Maybe a
    }

output :: forall r a. Boolean -> Maybe a -> Color -> HTML r a
output false _ _ = HH.text ""

output true selectOutput color =
  SE.circle
    [ SA.r 10.0
    , SA.fill $ Just color
    , SA.class_ "node-output"
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

renderNode :: forall h a. Input h a -> Actions a -> HTML h a
renderNode { nodeData: nodeData
, functionData
, labels
, colorMap
, hasOutput
, node
, nodeDataMap
, selectionStatus
} { select
, selectOutput
, selectInput
} =
  SE.g
    [ SA.transform [ SA.Translate (centerPosition !! d0) (centerPosition !! d1) ]
    , onMouseDown $ const select
    ]
    $ [ overlays maxRadius labels
      , SE.circle [ SA.r nodeRadius, SA.fill $ Just transparent ]
      ]
    <> arcs
    <> [ output
          hasOutput
          selectOutput
          $ fromMaybe transparent
          $ Map.lookup OutputPin colorMap
      ]
  where
  centerPosition = view _NodeDataPosition nodeData

  inputArcs = getArcs nodeDataMap nodeData node

  maxRadius = nodeRadius + (toNumber $ List.length inputArcs - 1) * inputLayerOffset

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

                        -- The edge to render
                        edge =
                          maybe mempty pure do
                            nodeId <- join $ getInputs node `List.index` index
                            targetData <- Map.lookup nodeId nodeDataMap
                            color <- Map.lookup (InputPin index) colorMap
                            let
                              targetPosition = view _NodeDataPosition targetData

                              inputPosition = vec2 (cos angle * radius) (sin angle * radius)
                            pure
                              $ renderEdge
                                  { from: inputPosition
                                  , to: targetPosition - centerPosition
                                  , color
                                  }

                        -- The actual svg for the input arc
                        inputSvg =
                          input
                            { arc
                            , spacing:
                              if List.length inputsLayer == 1 then
                                0.0
                              else
                                arcSpacing
                            , radius
                            , color: fromMaybe transparent $ Map.lookup (InputPin index) colorMap
                            }
                            $ selectInput index
                      in
                        inputSvg : edge
            )
        # join
        # List.toUnfoldable
