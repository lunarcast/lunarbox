module Lunarbox.Component.Editor.Node
  ( component
  , Query(..)
  , Output(..)
  , Input
  , State
  ) where

import Prelude
import Data.Array (catMaybes, mapWithIndex)
import Data.Int (toNumber)
import Data.Lens (Lens', over, set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_, raise)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown)
import Lunarbox.Data.Editor.FunctionData (FunctionData(..))
import Lunarbox.Data.Editor.Node (Node)
import Lunarbox.Data.Editor.Node.NodeData (NodeData(..), _NodeDataPosition, _NodeDataSelected, _NodeDataZPosition)
import Lunarbox.Data.Vector (Vec2)
import Lunarbox.Svg.Attributes (strokeWidth)
import Svg.Attributes (TextAnchor(..))
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { nodeData :: NodeData
    , node :: Node
    , selectable :: Boolean
    , functionData :: FunctionData
    , labels :: Array (Maybe String)
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
    SE.g []
      <<< mapWithIndex
          ( \index elem ->
              SE.g
                [ SA.transform
                    [ SA.Translate 0.0 $ toNumber $ (index + 1) * -20
                    ]
                ]
                [ elem ]
          )
      <<< catMaybes

  label scale text =
    SE.text
      [ SA.text_anchor AnchorMiddle
      , SA.x $ toNumber $ scale !! d0 / 2
      , SA.fill $ Just $ SA.RGB 63 196 255
      ]
      [ HH.text text ]

  render { selectable
  , functionData: FunctionData { image, scale }
  , nodeData: NodeData { position, selected }
  , labels
  } =
    SE.g
      [ SA.transform
          [ SA.Translate (position !! d0) (position !! d1) ]
      , onMouseDown $ const $ if selectable then Just $ SetSelection true else Nothing
      ]
      [ SE.circle
          [ SA.r $ toNumber $ scale !! d0 / 2 - 5
          , SA.cx $ toNumber $ scale !! d0 / 2
          , SA.cy $ toNumber $ scale !! d0 / 2
          , SA.fill $ Just $ SA.RGBA 0 0 0 0.0
          , SA.stroke $ Just $ if (selected && selectable) then SA.RGB 118 255 2 else SA.RGB 63 196 255
          , strokeWidth 5.0
          ]
      , overlays
          $ (label scale <$> _)
          <$> labels
      ]
