module Lunarbox.Component.Tutorial (component) where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Control.MonadZero (guard)
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Const (Const)
import Data.Default (def)
import Data.Either (Either(..), isRight)
import Data.Foldable (for_, traverse_)
import Data.Lens (over)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, fork, get, gets, mkComponent, mkEval, modify_, query, request, tell)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Gist (class ManageGists, fetchGist)
import Lunarbox.Capability.Resource.Project (class ManageProjects, createProject, getProject)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, completeTutorial, getTutorial)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Modal as Modal
import Lunarbox.Component.Tooltip as Tooltip
import Lunarbox.Component.Utils (className, maybeElement)
import Lunarbox.Config (Config)
import Lunarbox.Control.Monad.Dataflow.Interpreter (InterpreterContext)
import Lunarbox.Control.Monad.Dataflow.Interpreter.Interpret (termToRuntime)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.State as EditorState
import Lunarbox.Data.Gist (Gist)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Data.Tutorial (TutorialId, TutorialWithMetadata)
import Lunarbox.Data.TutorialConfig (TutorialSteps, getTutorialSteps)
import Lunarbox.Data.ValidateSolution (SolutionError(..), validateSolution)
import Network.RemoteData (RemoteData(..), fromEither)
import Random.LCG (randomSeed)
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
      , finished :: Boolean
      )
    }

data SlideModalAction
  = Next
  | Previous
  | Skip

data ResultModalAction
  = Continue
  | ToPlayground

data Action
  = Init
  | OpenCurrent
  | TryOpeningSlides
  | TestSolution
  -- | These 2 handle the closing of different modals
  | HandleSlideModalAction SlideModalAction
  | HandleResultModalAction ResultModalAction

type ChildSlots m
  = ( editor :: Slot Editor.Query Editor.Output Unit
    , slideModal :: Modal.Slot Action () SlideModalAction m Int
    , resultModal :: Modal.Slot Action () ResultModalAction m Unit
    )

resultModal :: forall m. Modal.InputType () ResultModalAction Action m
resultModal =
  { buttons: []
  , id: "result-modal"
  , onClose: Continue
  , title: "Test results"
  , content: \_ -> HH.text "Unimplemented"
  }

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
        , finished: false
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
    TestSolution -> do
      gets _.solution
        >>= traverse_ \solution -> do
            query (SProxy :: SProxy "editor") unit (request Editor.GetState)
              >>= traverse_ (go solution)
      where
      go solution state = do
        case lookupMain state, lookupMain solution, lookupMainType state, lookupMainType solution of
          Just baseTerm, Just solutionTerm, Just baseType, Just solutionType -> do
            seed <- liftEffect randomSeed
            result <-
              liftEffect
                $ validateSolution
                    { current:
                      { type': baseType
                      , value: baseTerm
                      }
                    , intended:
                      { type': solutionType
                      , value: solutionTerm
                      }
                    }
            let
              success = isRight result

              continue =
                { primary: true
                , text:
                  if success then
                    "Playground"
                  else
                    "Continue"
                , value: if success then ToPlayground else Continue
                }

              back = do
                guard success
                pure
                  { primary: false
                  , text: "Projects"
                  , value: Continue
                  }
            when success do
              modify_ _ { finished = true }
              gets _.id >>= void <<< completeTutorial
            void $ query (SProxy :: SProxy "resultModal") unit
              $ tell
              $ Modal.UpdateInput
              $ resultModal
                  { content =
                    \_ ->
                      HH.div [ className "tutorial__result-container" ]
                        [ HH.text
                            if success then
                              "Congratulations! You completed this tutorial!"
                            else
                              "It looks like your solution is not correct!"
                        , case result of
                            Left (CheckFailures failures) ->
                              HH.details [ className "tutorial__result" ]
                                $ [ HH.summary [ className "tutorial__result-summary" ]
                                      [ HH.text "The function doesn't have the correct behavior"
                                      ]
                                  ]
                                <> ( mkError
                                      <$> Array.fromFoldable failures
                                  )
                            Left (DifferentTypes t1 t2) ->
                              HH.div [ className "tutorial__result tutorial__result--type-error" ]
                                [ HH.text "Cannot match type "
                                , HH.br_
                                , HH.span [ className "tutorial__result-type" ]
                                    [ highlightTypeToHTML t2
                                    ]
                                , HH.br_
                                , HH.text " with type "
                                , HH.br_
                                , HH.span [ className "tutorial__result-type" ]
                                    [ highlightTypeToHTML t1
                                    ]
                                ]
                            _ -> HH.text ""
                        ]
                  , buttons = back <> [ continue ]
                  }
            void $ query (SProxy :: SProxy "resultModal") unit $ tell Modal.Open
            pure unit
          _, _, _, _ -> pure unit
        where
        mkError message =
          HH.div [ className "tutorial__result-error" ]
            [ HH.text message
            ]

        main :: Location
        main = AtFunction (FunctionName "main")

        lookupMainType :: EditorState.State -> Maybe Type
        lookupMainType s = Map.lookup main s.typeMap

        lookupMain :: EditorState.State -> Maybe RuntimeValue
        lookupMain s = fromTerm <$> term
          where
          term = Map.lookup main $ unwrap s.valueMap

          fromTerm t = termToRuntime ctx t
            where
            ctx =
              over _Newtype
                _ { overwrites = s.runtimeOverwrites }
                (def :: InterpreterContext Location)
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
    HandleResultModalAction a -> do
      finished <- gets _.finished
      when finished case a of
        Continue -> do
          navigate Projects
        ToPlayground ->
          query (SProxy :: _ "editor") unit (request Editor.GetState)
            >>= traverse_ \state ->
                createProject state
                  >>= traverse_ (navigate <<< Project)

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

  handleEditorOutput = const Nothing

  handleResultModalOutput = case _ of
    Modal.ClosedWith a -> Just $ HandleResultModalAction a
    Modal.BubbledAction a -> Just a

  render { tutorial, gist, steps, base, solution, currentSlide } = case tutorial, gist, steps, base, solution of
    Success tutorial', Success gist', Success steps', Success base', Success solution' ->
      HH.div_
        $ [ HH.main [ className "tutorial__editor" ]
              [ HH.slot (SProxy :: _ "editor") unit Editor.component base' handleEditorOutput
              ]
          , HH.aside [ className "tutorial__buttons" ]
              [ HH.button
                  [ className "tutorial__button tutorial__button--run"
                  , onClick $ const $ Just TestSolution
                  ]
                  [ Tooltip.tooltip "Check solution"
                      Tooltip.Left
                      HH.span
                      []
                      [ icon "play_arrow" ]
                  ]
              , HH.button
                  [ className "tutorial__button tutorial__button--hint"
                  , onClick $ const $ Just OpenCurrent
                  ]
                  [ Tooltip.tooltip
                      "See tutorial help"
                      Tooltip.Left
                      HH.span
                      []
                      [ HH.text "?" ]
                  ]
              ]
          ]
        <> slides
        <> [ HH.slot (SProxy :: SProxy "resultModal") unit Modal.component resultModal
              handleResultModalOutput
          ]
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
