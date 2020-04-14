module Lunarbox.Component.Editor.Node
  ( component
  , Query(..)
  , Output(..)
  , Input
  , State
  ) where

import Prelude
import Data.Array (catMaybes, mapWithIndex)
import Data.Array (toUnfoldable) as Array
import Data.Int (toNumber)
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_, raise)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown)
import Lunarbox.Capability.Editor.Node.NodeInput (Arc(..), fillWith)
import Lunarbox.Data.Editor.Constants (arcSpacing, arcWidth, nodeRadius)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs)
import Lunarbox.Data.Editor.Node (Node)
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected, _NodeDataZPosition)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Svg.Attributes (Linecap(..), arc, strokeDashArray, strokeLinecap, strokeWidth, transparent)
import Math (pi)
import Svg.Attributes (D(..), TextAnchor(..))
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { nodeData :: NodeData
    , node :: Node
    , selectable :: Boolean
    , functionData :: FunctionData
    , labels :: Array (Maybe String)
    , hasOutput :: Boolean
    }

-- Lenses
_nodeData :: Lens' State NodeData
_nodeData = prop (SProxy :: SProxy "nodeData")

_position :: Lens' State (Vec2 Number)
_position = _nodeData <<< _NodeDataPosition

_zPosition :: Lens' State Int
_zPosition = _nodeData <<< _NodeDataZPosition

_stateSelected :: Lens' State Boolean
_stateSelected = _nodeData <<< _NodeDataSelected

_selectable :: Lens' State Boolean
_selectable = prop (SProxy :: _ "selectable")

_labels :: Lens' State (Array (Maybe String))
_labels = prop (SProxy :: _ "labels")

data Action
  = SetSelection Boolean
  | Receive Input

data Query a
  = Unselect a
  | GetData (NodeData -> a)
  | Drag (Vec2 Number) a
  | SetZPosition Int a

type ChildSlots
  = ()

data Output
  = Selected

type Input
  = State

output :: forall r. Boolean -> HTML r Action
output false = HH.text ""

output true =
  SE.circle
    [ SA.r 10.0
    , SA.fill $ Just $ SA.RGB 118 255 0
    ]

displayArc :: forall r. Number -> Number -> Arc String -> HTML r Action
displayArc spacing radius (Arc start end _) =
  SE.path
    [ SA.d $ Abs <$> arc radius (start + spacing) (end - spacing)
    , SA.fill $ Just transparent
    , SA.stroke $ Just $ SA.RGB 63 196 255
    , strokeWidth arcWidth
    , strokeLinecap Round
    ]

constant :: forall r. HTML r Action
constant =
  SE.circle
    [ SA.r nodeRadius
    , SA.fill $ Just transparent
    , SA.stroke $ Just $ SA.RGB 176 112 107
    , strokeWidth arcWidth
    , strokeLinecap Butt
    , strokeDashArray [ pi * nodeRadius / 20.0 ]
    ]

component :: forall m. MonadEffect m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , receive = Just <<< Receive
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    SetSelection value -> do
      modify_ $ set _stateSelected value
      when (value == true) $ raise Selected
    Receive { labels } -> do
      modify_ $ set _labels labels

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    Unselect inner -> do
      modify_ $ set _stateSelected false
      pure $ Just inner
    Drag offest inner -> do
      selected <- gets $ view _stateSelected
      when selected $ modify_ $ over _position $ (+) offest
      pure $ Just inner
    -- This runs when the Scene component wants us to save the data
    GetData k -> do
      nodeData <- gets $ view _nodeData
      pure $ Just $ k nodeData
    SetZPosition value k -> do
      modify_ $ set _zPosition value
      pure $ Just k

  overlays :: Array (Maybe (HTML _ Action)) -> HTML _ Action
  overlays =
    SE.g [ SA.class_ "unselectable" ]
      <<< mapWithIndex
          ( \index elem ->
              SE.g
                [ SA.transform
                    [ SA.Translate 0.0 $ -nodeRadius + (toNumber $ (index + 1) * -20)
                    ]
                ]
                [ elem ]
          )
      <<< catMaybes

  label text =
    SE.text
      [ SA.text_anchor AnchorMiddle
      , SA.fill $ Just $ SA.RGB 63 196 255
      ]
      [ HH.text text ]

  render { selectable
  , nodeData: NodeData { position, selected }
  , functionData
  , labels
  , hasOutput
  } =
    let
      inputs = Array.toUnfoldable $ _.name <$> view _FunctionDataInputs functionData

      inputArcs = fillWith inputs Nil
    in
      SE.g
        [ SA.transform
            [ SA.Translate (position !! d0) (position !! d1) ]
        , onMouseDown $ const $ if selectable then Just $ SetSelection true else Nothing
        ]
        [ overlays
            $ (label <$> _)
            <$> labels
        , output hasOutput
        , if List.null inputArcs then
            constant
          else
            SE.g
              [ SA.transform [ SA.Rotate 90.0 0.0 0.0 ]
              ]
              $ displayArc (if List.length inputArcs == 1 then 0.0 else arcSpacing) nodeRadius
              <$> (List.toUnfoldable inputArcs)
        ]
