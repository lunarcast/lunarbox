module Lunarbox.Component.Tutorial (component) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Resource.Gist (class ManageGists, fetchGist)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, getTutorial)
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Config (Config)
import Lunarbox.Data.Gist (Gist)
import Lunarbox.Data.Tutorial (TutorialId, TutorialWithMetadata)
import Lunarbox.Data.TutorialConfig (TutorialSteps, getTutorialSteps)
import Network.RemoteData (RemoteData(..), fromEither)
import Record as Record

type Input r
  = ( id :: TutorialId | r )

type State
  = { 
    | Input
      ( tutorial :: RemoteData String TutorialWithMetadata
      , gist :: RemoteData String Gist
      , steps :: RemoteData String TutorialSteps
      )
    }

data Action
  = Init

type ChildSlots
  = ()

component ::
  forall m.
  MonadEffect m =>
  MonadAsk Config m =>
  ManageGists m =>
  ManageTutorials m => Component HH.HTML (Const Void) { | Input () } Void m
component =
  mkComponent
    { initialState:
      Record.merge
        { tutorial: NotAsked
        , gist: NotAsked
        , steps: NotAsked
        }
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Init -> do
      id <- gets _.id
      response <- getTutorial id
      modify_ _ { tutorial = fromEither response }
      -- | Debug only
      for_ response \{ content } -> do
        gist <- fetchGist content
        modify_ _ { gist = fromEither gist }
        for_ gist \gist' -> do
          let
            steps = getTutorialSteps gist'
          modify_ _ { steps = fromEither steps }

  render { tutorial, gist, steps } = case tutorial, gist, steps of
    Success tutorial', Success gist', Success steps' -> HH.text "Everything worked fine :D"
    Failure err, _, _ -> error err
    _, Failure err, _ -> error err
    _, _, Failure err -> error err
    _, _, _ -> loading
