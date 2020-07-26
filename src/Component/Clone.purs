module Lunarbox.Component.Clone (component) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, cloneProject)
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.Route (Route(..))
import Network.RemoteData (RemoteData(..))
import Record as Record

data Action
  = Init

type Input r
  = ( targetId :: ProjectId | r )

type State
  = { | Input ( newId :: RemoteData String ProjectId ) }

component :: forall m q o. ManageProjects m => Navigate m => Component HH.HTML q { | Input () } o m
component =
  mkComponent
    { initialState: Record.merge { newId: NotAsked }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM State Action () o m Unit
  handleAction = case _ of
    Init -> do
      targetId <- gets _.targetId
      response <- cloneProject targetId
      case response of
        Left message -> modify_ _ { newId = Failure message }
        Right newId -> navigate $ Project newId

  render { newId } = case newId of
    Failure message -> error message
    _ -> loading
