module Lunarbox.Component.Project
  ( component
  , Input
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.State (gets, modify_)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Lens (Lens', set, view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Debug.Trace (spy)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject, saveProject)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Error (error)
import Lunarbox.Component.HOC.Connect as Connect
import Lunarbox.Component.Loading (loading)
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

data Action
  = Init
  | Save Json
  | Receive { | Connect.WithCurrentUser Input }

type ChildSlots
  = ( editor :: forall query. Slot query Editor.Output Unit )

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
  handleAction :: Action -> HalogenM (State m) Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      id <- gets $ view _id
      response <- getProject id
      modify_ $ set _projectData $ fromEither response
    Receive userData -> modify_ $ Record.merge userData
    Save json -> do
      id <- gets $ view _id
      response <- saveProject id json
      case response of
        Left error -> modify_ $ set _projectData $ Failure error
        _ -> pure unit

  handleEditorOutput :: Editor.Output -> Maybe Action
  handleEditorOutput = case _ of
    Editor.Save state -> Just $ Save state

  render { projectData, currentUser } = case projectData of
    NotAsked -> loading
    Loading -> loading
    Failure text -> error text
    Success state -> case currentUser of
      Nothing -> error "not logged in"
      Just { isAdmin } -> slot (SProxy :: _ "editor") unit Editor.component (Record.merge { isAdmin } state) handleEditorOutput
