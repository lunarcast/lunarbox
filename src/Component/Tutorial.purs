module Lunarbox.Component.Tutorial (component) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.MonadZero (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, fork, get, gets, mkComponent, mkEval, modify_, query, tell)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Gist (class ManageGists, fetchGist)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, getTutorial)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Modal as Modal
import Lunarbox.Component.Utils (className, maybeElement)
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.State as EditorState
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
      , base :: RemoteData String EditorState.State
      , solution :: RemoteData String EditorState.State
      , currentSlide :: Int
      )
    }

data SlideModalAction
  = Next
  | Previous

data Action
  = Init
  | HandleSlideModalAction SlideModalAction

type ChildSlots m
  = ( editor :: Slot (Const Void) Editor.Output Unit
    , slideModal :: Modal.Slot Action () SlideModalAction m Int
    )

component ::
  forall m.
  MonadEffect m =>
  MonadReader Config m =>
  MonadAff m =>
  Navigate m =>
  ManageGists m =>
  ManageProjects m =>
  ManageTutorials m => Component HH.HTML (Const Void) { | Input () } Void m
component =
  mkComponent
    { initialState:
      Record.merge
        { tutorial: NotAsked
        , gist: NotAsked
        , steps: NotAsked
        , base: NotAsked
        , solution: NotAsked
        , currentSlide: 0
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
  _slideModal :: SProxy "slideModal"
  _slideModal = SProxy

  handleAction :: Action -> HalogenM State Action (ChildSlots m) Void m Unit
  handleAction = case _ of
    Init -> do
      id <- gets _.id
      response <- getTutorial id
      modify_ _ { tutorial = fromEither response }
      for_ response \response' ->
        for_ setupFetchers \f -> fork $ f response'
      void
        $ fork do
            liftAff $ delay $ Milliseconds 100.0
            void $ query _slideModal 0 $ tell Modal.Open
    HandleSlideModalAction action -> do
      { currentSlide, steps } <- get
      for_ steps \{ steps: steps' } -> do
        case action of
          Next ->
            when (Array.length steps' > currentSlide + 1) do
              modify_ _ { currentSlide = currentSlide + 1 }
              openSlide (currentSlide + 1)
          Previous ->
            unless (currentSlide == 0) do
              modify_ _ { currentSlide = currentSlide - 1 }
              openSlide (currentSlide - 1)

  openSlide value = void $ query _slideModal value $ tell Modal.Open

  setupFetchers = [ fetchBase, fetchSolution, fetchSteps ]

  fetchBase { base } = do
    base' <- getProject base
    modify_ _ { base = fromEither base' }

  fetchSolution { solution } = do
    solution' <- getProject solution
    modify_ _ { solution = fromEither solution' }

  fetchSteps { content } = do
    gist <- fetchGist content
    modify_ _ { gist = fromEither gist }
    for_ gist \gist' -> do
      let
        steps = getTutorialSteps gist'
      modify_ _ { steps = fromEither steps }

  render { tutorial, gist, steps, base, solution, currentSlide } = case tutorial, gist, steps, base, solution of
    Success tutorial', Success gist', Success steps', Success base', Success solution' ->
      HH.div_
        [ HH.main [ className "tutorial__editor" ]
            [ HH.slot (SProxy :: _ "editor") unit Editor.component base' (const Nothing)
            ]
        , HH.slot _slideModal currentSlide Modal.component slideModal handleSlideModalOutput
        ]
      where
      currentStep = steps'.steps !! currentSlide

      next =
        { text:
          if (currentSlide + 1 >= Array.length steps'.steps) then
            "Done"
          else
            "Next"
        , value: Next
        , primary: true
        }

      previous = do
        guard (currentSlide /= 0)
        pure
          $ { text: "Previous"
            , value: Previous
            , primary: false
            }

      slideModal =
        { buttons:
          previous
            <> [ next
              ]
        , content: \_ -> maybeElement currentStep (HH.text <<< _.content)
        , id: "slide-modal"
        , onClose: Next
        , title: maybe "Cannot find title" _.title currentStep
        }

      handleSlideModalOutput = case _ of
        Modal.ClosedWith value -> Just $ HandleSlideModalAction value
        Modal.BubbledAction action -> Just action
    Failure err, _, _, _, _ -> error err
    _, Failure err, _, _, _ -> error err
    _, _, Failure err, _, _ -> error err
    _, _, _, Failure err, _ -> error err
    _, _, _, _, Failure err -> error err
    _, _, _, _, _ -> loading
