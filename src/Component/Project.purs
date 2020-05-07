module Lunarbox.Component.Project
  ( component
  , Input
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (gets, modify_)
import Data.Either (Either(..))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject, saveProject)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.HOC.Connect as Connect
import Lunarbox.Config (Config)
import Lunarbox.Data.Profile (Profile)
import Lunarbox.Data.ProjectId (ProjectId)
import Network.RemoteData (RemoteData(..), fromEither)
import Record as Record

type State m
  = { id :: ProjectId
    , projectData :: RemoteData String (Editor.EditorState m)
    , currentUser :: Maybe Profile
    }

type Input
  = ( id :: ProjectId )

-- Lenses
_projectData :: forall m. Lens' (State m) (RemoteData String (Editor.EditorState m))
_projectData = prop (SProxy :: _ "projectData")

_id :: forall m. Lens' (State m) ProjectId
_id = prop (SProxy :: _ "id")

data Action m
  = Init
  | Save (Editor.EditorState m)
  | Receive { | Connect.WithCurrentUser Input }

type ChildSlots m
  = ( editor :: forall query. Slot query (Editor.Output m) Unit )

component ::
  forall m q o.
  MonadEffect m =>
  MonadAff m =>
  MonadAsk Config m =>
  MonadReader Config m =>
  ManageProjects m =>
  Navigate m =>
  Component HH.HTML q { | Input } o m
component =
  Connect.component
    $ mkComponent
        { initialState: Record.merge { projectData: NotAsked }
        , render
        , eval:
          mkEval
            $ defaultEval
                { handleAction = handleAction
                , initialize = Just Init
                , receive = Just <<< Receive
                }
        }
  where
  handleAction :: Action m -> HalogenM (State m) (Action m) (ChildSlots m) o m Unit
  handleAction = case _ of
    Init -> do
      id <- gets $ view _id
      response <- getProject id
      modify_ $ set _projectData $ fromEither response
    Receive userData -> modify_ $ Record.merge userData
    Save state -> do
      response <- saveProject state
      case response of
        Left error -> modify_ $ set _projectData $ Failure error
        _ -> pure unit

  handleEditorOutput :: Editor.Output m -> Maybe (Action m)
  handleEditorOutput = case _ of
    Editor.Save state -> Just $ Save state

  loading = HH.text "loading"

  render { projectData } = case projectData of
    NotAsked -> loading
    Loading -> loading
    Failure text -> HH.text $ "An error occured " <> text
    Success state -> slot (SProxy :: _ "editor") unit Editor.component state handleEditorOutput
