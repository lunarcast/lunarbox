module Lunarbox.Component.Project (component) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.State (get, modify_)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML as HH
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Config (Config)
import Lunarbox.Data.ProjectId (ProjectId)
import Network.RemoteData (RemoteData(..), fromEither)

type State m
  = { id :: ProjectId
    , projectData :: RemoteData String (Editor.EditorState m)
    }

-- Lenses
_projectData :: forall m. Lens' (State m) (RemoteData String (Editor.EditorState m))
_projectData = prop (SProxy :: _ "projectData")

data Action
  = Init

type Output
  = Void

type ChildSlots
  = ( editor :: Slot Editor.Query Void Unit )

component :: forall m q. MonadEffect m => MonadAsk Config m => ManageProjects m => Component HH.HTML q (State m) Output m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM (State m) Action ChildSlots Output m Unit
  handleAction = case _ of
    Init -> do
      { projectData, id } <- get
      case projectData of
        Failure _ -> pure unit
        Success _ -> pure unit
        _ -> do
          response <- getProject id
          modify_ $ set _projectData $ fromEither response

  render _ = HH.text "unimplemented"
