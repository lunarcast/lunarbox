module Lunarbox.Component.Editor.Add
  ( State
  , Query(..)
  , Input
  , component
  , Output(..)
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.MonadZero (guard)
import Data.Int (toNumber)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..), Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, put, raise)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Editor.Node as NodeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataScale)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Node.NodeDescriptor (describe)
import Lunarbox.Data.Editor.Project (Project)
import Svg.Attributes as SA
import Svg.Elements as SE

type State
  = { project :: Project FunctionData NodeData
    , currentFunction :: Maybe FunctionName
    }

_currentFunction :: Lens' State (Maybe FunctionName)
_currentFunction = prop (SProxy :: _ "currentFunction")

_project :: Lens' State (Project FunctionData NodeData)
_project = prop (SProxy :: _ "project")

data Action
  = SetState State
  | AddNode FunctionName
  | SelectFunction FunctionName

data Query a
  = Void

data Output
  = SelectedFunction FunctionName
  | AddedNode FunctionName

type ChildSlots
  = ( node :: Slot NodeC.Query NodeC.Output FunctionName )

type Input
  = State

nodeInput :: FunctionName -> FunctionData -> NodeC.Input
nodeInput name functionData =
  { selectable: false
  , nodeData: mempty
  , node: ComplexNode { inputs: mempty, function: name }
  , functionData
  , labels: mempty
  }

component :: forall m. MonadEffect m => MonadAsk Config m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , receive = Just <<< SetState
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    SelectFunction name -> raise $ SelectedFunction name
    AddNode functionName -> raise $ AddedNode functionName
    SetState state -> put state

  makeNode (Tuple { functionData, name } { isUsable, isEditable }) =
    let
      scale = view _FunctionDataScale functionData

      side = toNumber $ max (scale !! d0) (scale !! d1)
    in
      HH.div [ className "node" ]
        [ SE.svg
            [ SA.width 75.0
            , SA.height 75.0
            , SA.viewBox 0.0 0.0 side side
            ]
            [ slot
                (SProxy :: _ "node")
                name
                NodeC.component
                (nodeInput name functionData)
                $ const Nothing
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
                    , onClick $ const $ guard isUsable $> AddNode name
                    ]
                    [ icon "add" ]
                , HH.div
                    [ HP.classes $ ClassName <$> ("active" <$ guard isEditable)
                    , onClick $ const $ guard isEditable $> SelectFunction name
                    ]
                    [ icon "edit"
                    ]
                ]
            ]
        ]

  render { project, currentFunction } =
    container "nodes"
      $ makeNode
      <$> describe currentFunction project
