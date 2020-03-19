module Lunarbox.Component.Editor.Scene where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Graph (lookup) as G
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Component.Editor.Node as Node
import Lunarbox.Config (Config)
import Lunarbox.Data.Graph (entries) as G
import Lunarbox.Data.Project (FunctionName, NodeGroup(..), NodeId, Project, VisualFunction(..))
import Lunarbox.Page.Editor.EmptyEditor (emptyEditor)
import Svg.Elements as SE

type State
  = { project :: Project
    , currentFunction :: Maybe FunctionName
    }

data Action
  = SomeAction

data Query a
  = SelectFunction (Maybe FunctionName)

type Output
  = Void

type ChildSlots
  = ( node :: Slot Node.Query Void NodeId )

type Input
  = State

component :: forall m. MonadEffect m => MonadAsk Config m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    SomeAction -> pure unit

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    SelectFunction name -> do
      modify_ (_ { currentFunction = name })
      pure Nothing

  render { project, currentFunction: maybeCurrentFunction } =
    fromMaybe
      (emptyEditor unit) do
      currentFunction <- maybeCurrentFunction
      function <- G.lookup currentFunction project.functions
      case function of
        NativeVF _ -> Nothing
        DataflowFunction (NodeGroup { nodes: nodeGraph }) -> Just $ SE.svg [] ((\(Tuple id node) -> HH.slot (SProxy :: _ "node") id Node.component {} absurd) <$> nodes)
          where
          nodes = G.entries nodeGraph
