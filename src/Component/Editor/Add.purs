module Lunarbox.Component.Editor.Add
  ( add
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Default (def)
import Data.Lens (view)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen (ClassName(..))
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Editor.Node (node)
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Data.Editor.Constants (arcWidth, nodeRadius)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Node.NodeDescriptor (FunctionGraphNode, NodeDescriptor, describe)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (Project)
import Svg.Attributes as SA
import Svg.Elements as SE

type Input
  = { project :: Project FunctionData NodeData
    , currentFunction :: Maybe FunctionName
    }

type Actions a
  = { edit :: FunctionName -> Maybe a
    , addNode :: FunctionName -> Maybe a
    , select :: Maybe a
    }

nodeInput :: FunctionName -> FunctionData -> NodeC.Input
nodeInput name functionData =
  { nodeData: def
  , node: ComplexNode { inputs: mempty, function: name }
  , functionData
  , labels: mempty
  , hasOutput: false
  , colorMap:
    Map.fromFoldable
      $ Array.mapWithIndex
          (\index _ -> Tuple (InputPin index) $ SA.RGB 176 112 107)
      $ view _FunctionDataInputs functionData
  }

makeNode :: forall h a. Tuple FunctionGraphNode NodeDescriptor -> Actions a -> HTML h a
makeNode (Tuple { functionData, name } { isUsable, isEditable }) { edit, addNode, select } =
  HH.div [ className "node" ]
    [ SE.svg
        [ SA.width 75.0
        , SA.height 75.0
        , let size = arcWidth + nodeRadius in SA.viewBox (-size) (-size) (2.0 * size) (2.0 * size)
        ]
        [ node
            (nodeInput name functionData)
            { select }
        ]
    , container "node-data"
        [ container "node-text"
            [ container "node-name"
                [ HH.text $ show name
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

add :: forall h a. Input -> Actions a -> HTML h a
add { project, currentFunction } actions =
  container "nodes"
    $ (flip makeNode) actions
    <$> describe currentFunction project
