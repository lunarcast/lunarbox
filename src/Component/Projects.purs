module Lunarbox.Component.Projects
  ( component
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Lens (Lens', preview, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, HalogenM, defaultEval, gets, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, cloneProject, createProject, deleteProject, getProjects)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, createTutorial, deleteTutorial)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Tabs as Tabs
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Component.WithLogo (withLogo)
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.State (emptyState)
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.ProjectList (ProjectList, ProjectOverview)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Data.Tutorial (TutorialId)
import Network.RemoteData (RemoteData(..), _Success, fromEither)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Tab
  = PersonalProjects
  | Examples
  | Tutorials

derive instance eqTab :: Eq Tab

instance showTab :: Show Tab where
  show PersonalProjects = "Projects"
  show Examples = "Examples"
  show Tutorials = "Tutorials"

type State
  = { projectList :: RemoteData String ProjectList
    , search :: String
    , currentTab :: Tab
    }

-- Lenses
_projectList :: Lens' State (RemoteData String ProjectList)
_projectList = prop (SProxy :: _ "projectList")

_search :: Lens' State String
_search = prop (SProxy :: _ "search")

data Action
  = Initialize
  | OpenProject ProjectId
  | DeleteProject ProjectId MouseEvent
  | DeleteTutorial TutorialId MouseEvent
  | CreateProject
  | CreateTutorial
  | CloneProject ProjectId
  | Search String
  | Navigate Route
  | SetTab Tab

type Output
  = Void

type ChildSlots
  = ()

type Input
  = {}

component ::
  forall m q.
  MonadEffect m =>
  ManageProjects m =>
  ManageTutorials m =>
  MonadAsk
    Config
    m =>
  Navigate m => Component HH.HTML q Input Output m
component =
  mkComponent
    { initialState:
      const
        { projectList: NotAsked
        , search: ""
        , currentTab: PersonalProjects
        }
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
    Initialize -> do
      projectList <- getProjects
      modify_ _ { projectList = fromEither projectList }
    Search term -> modify_ _ { search = term }
    Navigate route -> navigate route
    OpenProject id -> navigate $ Project id
    SetTab tab -> modify_ _ { currentTab = tab }
    CloneProject id -> do
      response <- cloneProject id
      case response of
        Right cloneId -> navigate $ Project cloneId
        Left err -> modify_ _ { projectList = Failure err }
    DeleteProject id event -> do
      liftEffect $ stopPropagation $ MouseEvent.toEvent event
      oldProjects <- gets $ preview $ _projectList <<< _Success
      modify_ $ set _projectList Loading
      response <- deleteProject id
      case response of
        Right _ ->
          modify_
            $ set _projectList
            $ case oldProjects of
                Just value ->
                  Success
                    $ value
                        { userProjects = filter ((_ /= id) <<< _.id) value.userProjects }
                Nothing -> Failure "Cannot find projec list"
        Left err -> modify_ $ set _projectList $ Failure err
    DeleteTutorial id event -> do
      liftEffect $ stopPropagation $ MouseEvent.toEvent event
      oldProjects <- gets $ preview $ _projectList <<< _Success
      modify_ $ set _projectList Loading
      deleteTutorial id
        >>= case _ of
            Right _ -> modify_ $ set _projectList newProjectList
              where
              newProjectList = case oldProjects of
                Just value ->
                  Success
                    $ value
                        { tutorials =
                          filter ((_ /= id) <<< _.id)
                            value.tutorials
                        }
                Nothing -> Failure "Cannot find old project list"
            Left err -> modify_ $ set _projectList $ Failure err
    CreateProject -> do
      -- Display a loading message
      modify_ $ set _projectList Loading
      state <- emptyState
      response <- createProject state
      case response of
        Right id -> navigate $ Project id
        Left err -> modify_ $ set _projectList $ Failure err
    CreateTutorial -> do
      modify_ $ set _projectList Loading
      response <- createTutorial
      case response of
        Right id -> navigate $ EditTutorial id
        Left err -> modify_ $ set _projectList $ Failure err

  renderProject isExample { name, id, metadata: { functionCount, nodeCount } } =
    HH.div [ className "project", onClick $ const $ Just $ (if isExample then CloneProject else OpenProject) id ]
      [ HH.div [ className "project__name no-overflow" ] [ HH.text name ]
      , HH.div [ className "project__data" ]
          [ HH.div [ className "project__data-item" ]
              [ icon "functions"
              , HH.text $ show functionCount
              ]
          , HH.div [ className "project__data-item" ]
              [ icon "track_changes"
              , HH.text $ show nodeCount
              ]
          , whenElem (not isExample) \_ ->
              HH.div
                [ className "project__data-icon"
                , onClick $ Just <<< DeleteProject id
                ]
                [ icon "delete"
                ]
          ]
      ]

  renderProjectList :: Boolean -> Array { | ProjectOverview } -> Array (HH.HTML _ _) -> HH.HTML _ _
  renderProjectList areExamples projects extraRows =
    HH.div
      [ className "projects__list" ]
      $ extraRows
      <> list
    where
    list = renderProject areExamples <$> projects

  withRemoteData remoteData f = case remoteData of
    NotAsked -> loading
    Loading -> loading
    Failure err -> HH.text $ "error " <> err
    Success result -> f result

  examplesExist = case _ of
    Success { exampleProjects } -> not $ Array.null exampleProjects
    _ -> true

  examplesHtml { projectList, search } = withRemoteData projectList go
    where
    go { exampleProjects } = renderProjectList true projects []
      where
      projects = sortBySearch _.name search exampleProjects

  newProject a =
    HH.div
      [ className "project project--new"
      , onClick $ const
          $ Just a
      ]
      [ icon "add" ]

  projectsHtml { projectList, search } = withRemoteData projectList go
    where
    go { userProjects } = renderProjectList false projects [ createProject ]
      where
      projects = sortBySearch _.name search userProjects

      createProject = newProject CreateProject

  tutorialsHtml { projectList, search } = withRemoteData projectList go
    where
    go { tutorials } =
      HH.div [ className "projects__list" ]
        $ [ newProject CreateProject ]
        <> (mkItem <$> tutorials)
      where
      mkItem { name, completed, id } =
        HH.div [ className "project", onClick $ const $ Just CreateTutorial ]
          [ HH.div [ className "project__name no-overflow" ] [ HH.text name ]
          , HH.div [ className "project__data" ]
              [ whenElem completed \_ ->
                  HH.div
                    [ className "project__data-icon"
                    ]
                    [ icon "verified" ]
              , HH.div
                  [ className "project__data-icon"
                  , onClick $ Just <<< DeleteTutorial id
                  ]
                  [ icon "delete"
                  ]
              ]
          ]

  goBack =
    HH.div
      [ HE.onClick $ const
          $ Just
          $ Navigate Home
      , className "projects__back"
      ]
      [ icon "arrow_back_ios" ]

  searchBar search =
    HH.input
      [ HP.value search
      , HP.placeholder "Search"
      , HE.onValueInput $ Just <<< Search
      , className "projects__search-bar"
      ]

  render state@{ search, currentTab, projectList } =
    HH.div [ className "projects" ]
      [ withLogo
          $ HH.div [ className "projects__container" ]
              [ Tabs.component
                  { currentTab
                  , headerStart: Just goBack
                  , headerEnd: Just $ searchBar search
                  , setTab: Just <<< SetTab
                  , tabs:
                    [ { name: PersonalProjects
                      , content: projectsHtml state
                      }
                    ]
                      <> examplesTab
                      <> [ { name: Tutorials
                          , content: tutorialsHtml state
                          }
                        ]
                  }
              ]
      ]
    where
    examplesTab = do
      guard (examplesExist projectList)
      [ { name: Examples
        , content: examplesHtml state
        }
      ]
