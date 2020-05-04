module Lunarbox.Component.Projects (component) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Lunarbox.Config (Config)
import Network.RemoteData (RemoteData(..))

type State
  = { projects :: RemoteData String String
    }

data Action
  = Initialize

type Output
  = Void

type ChildSlots
  = ()

type Input
  = {}

component :: forall m q. MonadEffect m => MonadAsk Config m => Component HH.HTML q Input Output m
component =
  mkComponent
    { initialState: const { projects: NotAsked }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize -> pure unit

  loading :: forall h a. HH.HTML h a
  loading = HH.text "loading"

  render { projects } = case projects of
    NotAsked -> loading
    Loading -> loading
    Failure err -> HH.text $ "error " <> err
    Success succ -> HH.text $ "succes " <> succ
