module Lunarbox.Component.Editor.Node
  ( node
  , Input
  ) where

import Prelude
import Data.Array (toUnfoldable) as Array
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onMouseDown)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..), fillWith)
import Lunarbox.Component.Editor.Node.Input (input)
import Lunarbox.Component.Editor.Node.Overlays (overlays)
import Lunarbox.Data.Editor.Constants (arcSpacing, arcWidth, nodeRadius)
import Lunarbox.Data.Editor.FunctionData (FunctionData(..))
import Lunarbox.Data.Editor.Node (Node)
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..))
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Svg.Attributes (Linecap(..), strokeDashArray, strokeLinecap, strokeWidth, transparent)
import Math (pi)
import Svg.Attributes (Color)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input h a
  = { nodeData :: NodeData
    , node :: Node
    , labels :: Array (HTML h a)
    , functionData :: FunctionData
    , colorMap :: Map Pin SA.Color
    , hasOutput :: Boolean
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

node :: forall h a. Input h a -> Actions a -> HTML h a
node { nodeData: NodeData { position }
, functionData: FunctionData { inputs }
, labels
, colorMap
, hasOutput
} { select
, selectOutput
, selectInput
} =
  SE.g
    [ SA.transform [ SA.Translate (position !! d0) (position !! d1) ]
    , onMouseDown $ const select
    ]
    [ overlays labels
    , SE.circle [ SA.r nodeRadius, SA.fill $ Just transparent ]
    , output
        hasOutput
        selectOutput
        $ fromMaybe transparent
        $ Map.lookup OutputPin colorMap
    , let
        inputNames = Array.toUnfoldable $ _.name <$> inputs

        inputArcs = fillWith inputNames Nil
      in
        if List.null inputArcs then
          constant
        else
          SE.g
            [ SA.transform [ SA.Rotate 90.0 0.0 0.0 ]
            ]
            $ ( \arc@(Arc _ _ name) ->
                  let
                    maybeIndex = List.findIndex (name == _) inputNames
                  in
                    input
                      { arc
                      , spacing:
                        if List.length inputArcs == 1 then
                          0.0
                        else
                          arcSpacing
                      , radius: nodeRadius
                      , color:
                        fromMaybe transparent do
                          index <- maybeIndex
                          Map.lookup (InputPin index) colorMap
                      }
                      $ maybeIndex
                      >>= selectInput
              )
            <$> List.toUnfoldable inputArcs
    ]
