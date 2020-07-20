module Lunarbox.Component.Tutorial (component) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.MonadZero (guard)
import Data.Array ((!!), (..))
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
import Halogen.HTML.Events (onClick)
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Gist (class ManageGists, fetchGist)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProject)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, getTutorial)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Icon (icon)
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
  | Skip

data Action
  = Init
  | HandleSlideModalAction SlideModalAction
  | OpenCurrent
  | TryOpeningSlides

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
      handleAction TryOpeningSlides
    HandleSlideModalAction action -> do
      { currentSlide, steps } <- get
      for_ steps \{ steps: steps' } -> do
        case action of
          Skip -> pure unit
          Next ->
            when (Array.length steps' > currentSlide + 1) do
              modify_ _ { currentSlide = currentSlide + 1 }
              handleAction OpenCurrent
          Previous ->
            unless (currentSlide == 0) do
              modify_ _ { currentSlide = currentSlide - 1 }
              handleAction OpenCurrent
    OpenCurrent -> do
      a <- gets _.currentSlide
      gets _.currentSlide >>= openSlide
    TryOpeningSlides -> do
      state <- get
      case state.tutorial, state.gist, state.steps, state.base, state.solution of
        Success _, Success _, Success _, Success _, Success _ -> handleAction OpenCurrent
        Failure _, _, _, _, _ -> pure unit
        _, Failure _, _, _, _ -> pure unit
        _, _, Failure _, _, _ -> pure unit
        _, _, _, Failure _, _ -> pure unit
        _, _, _, _, Failure _ -> pure unit
        _, _, _, _, _ ->
          void
            $ fork do
                liftAff $ delay $ Milliseconds 300.0
                handleAction TryOpeningSlides

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
        $ [ HH.main [ className "tutorial__editor" ]
              [ HH.slot (SProxy :: _ "editor") unit Editor.component base' (const Nothing)
              ]
          , HH.button
              [ className "tutorial__hint-button"
              , onClick $ const $ Just OpenCurrent
              ]
              [ icon "help"
              ]
          ]
        <> slides
      where
      slides =
        (0 .. (slideCount - 1))
          <#> mkModal

      slideCount = Array.length steps'.steps

      mkModal slideIndex =
        HH.slot _slideModal
          slideIndex
          Modal.component
          slideModal
          handleSlideModalOutput
        where
        slide = steps'.steps !! slideIndex

        next =
          { text:
            if (slideIndex + 1 >= Array.length steps'.steps) then
              "Done"
            else
              "Next"
          , value: Next
          , primary: true
          }

        previous = do
          guard (slideIndex /= 0)
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
          , content:
            \_ ->
              maybeElement slide \{ content } ->
                HH.div
                  [ className
                      if slideIndex == currentSlide then
                        "tutorial__slide tutorial__slide--active"
                      else
                        "tutorial__slide"
                  ]
                  [ HH.text content ]
          , id: "slide-modal-" <> show slideIndex
          , onClose: Skip
          , title: maybe "Cannot find title" _.title slide
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
