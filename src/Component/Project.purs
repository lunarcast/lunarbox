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
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, mkComponent, mkEval)
import Halogen.HTML (slot)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject, saveProject)
import Lunarbox.Data.Editor.State (State) as EditorState
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Error (error)
import Lunarbox.Component.HOC.Connect as Connect
import Lunarbox.Component.Loading (loading)
import Lunarbox.Config (Config)
import Lunarbox.Data.Profile (Profile)
import Lunarbox.Data.ProjectId (ProjectId)
import Network.RemoteData (RemoteData(..), fromEither)
import Record as Record

type Input r
  = ( id :: ProjectId | r )

type State
  = { 
    | Input
      ( projectData :: RemoteData String EditorState.State
      , currentUser :: Maybe Profile
      )
    }

-- Lenses
_projectData :: Lens' State (RemoteData String EditorState.State)
_projectData = prop (SProxy :: _ "projectData")

_id :: forall r. Lens' { | Input r } ProjectId
_id = prop (SProxy :: _ "id")

data Action
  = Init
  | Save Json
  | Receive { | Connect.WithCurrentUser (Input ()) }

type ChildSlots
  = ( editor :: Slot Editor.Query Editor.Output Unit )

component ::
  forall m q o.
  MonadEffect m =>
  MonadAff m =>
  MonadAsk Config m =>
  MonadReader Config m =>
  ManageProjects m =>
  Navigate m =>
  Component HH.HTML q { | Input () } o m
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
  handleAction :: Action -> HalogenM State Action ChildSlots o m Unit
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
    Editor.StateEmit _ -> Nothing

  render { projectData, currentUser } = case projectData of
    NotAsked -> loading
    Loading -> loading
    Failure text -> error text
    Success state -> case currentUser of
      Nothing -> error "not logged in"
      Just { isAdmin } -> slot (SProxy :: _ "editor") unit Editor.component (state { isAdmin = isAdmin }) handleEditorOutput
