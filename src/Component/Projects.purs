module Lunarbox.Component.Projects
  ( component
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
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
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Tabs as Tabs
import Lunarbox.Component.Utils (className, container, whenElem)
import Lunarbox.Component.WithLogo (withLogo)
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.State (emptyState)
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.ProjectList (ProjectList, ProjectOverview)
import Lunarbox.Data.Route (Route(..))
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
  | CreateProject
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
      modify_ $ set _projectList $ fromEither projectList
    Search term -> modify_ $ set _search term
    Navigate route -> navigate route
    OpenProject id -> navigate $ Project id
    SetTab tab -> modify_ _ { currentTab = tab }
    CloneProject id -> do
      response <- cloneProject id
      case response of
        Right cloneId -> navigate $ Project cloneId
        Left err -> modify_ $ set _projectList $ Failure err
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
                Just { exampleProjects, userProjects } ->
                  Success
                    { exampleProjects, userProjects: filter ((_ /= id) <<< _.id) userProjects }
                Nothing -> Failure "Cannot find projec list"
        Left err -> modify_ $ set _projectList $ Failure err
    CreateProject -> do
      -- Display a loading message
      modify_ $ set _projectList Loading
      state <- emptyState
      response <- createProject state
      case response of
        Right id -> navigate $ Project id
        Left err -> modify_ $ set _projectList $ Failure err

  renderProject isExample { name, id, metadata: { functionCount, nodeCount } } =
    HH.div [ className "project", onClick $ const $ Just $ (if isExample then CloneProject else OpenProject) id ]
      [ HH.div [ className "project-name no-overflow" ] [ HH.text name ]
      , HH.div [ className "project-data" ]
          [ HH.div [ className "function-count" ]
              [ icon "functions"
              , HH.text $ show functionCount
              ]
          , HH.div [ className "node-count" ]
              [ icon "track_changes"
              , HH.text $ show nodeCount
              ]
          , whenElem (not isExample) \_ ->
              HH.div
                [ className "node-icon"
                , onClick $ Just <<< DeleteProject id
                ]
                [ icon "delete"
                ]
          ]
      ]

  renderProjectList :: Boolean -> String -> Array { | ProjectOverview } -> Array (HH.HTML _ _) -> HH.HTML _ _
  renderProjectList areExamples title projects buttons =
    container "project-list"
      [ container "list-header"
          $ [ container "list-title" [ HH.text title ]
            ]
          <> buttons
      , container
          "list-items"
          $ renderProject areExamples
          <$> projects
      ]

  listButton :: String -> Action -> HH.HTML _ _
  listButton name handleClick =
    HH.div
      [ className "list-button"
      , onClick $ const $ Just handleClick
      ]
      [ icon name ]

  renderProjects { projectList, search } = case projectList of
    NotAsked -> pure loading
    Loading -> pure loading
    Failure err -> pure $ HH.text $ "error " <> err
    Success { userProjects, exampleProjects } ->
      [ renderProjectList false "Projects" (order $ Array.reverse userProjects) [ listButton "add" CreateProject ]
      , renderProjectList true "Examples" (order $ Array.reverse exampleProjects) []
      ]
    where
    order = sortBySearch _.name search

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

  render { search, currentTab } =
    container "projects-container"
      [ withLogo
          $ container "projects"
              [ Tabs.component
                  { currentTab
                  , headerStart: Just goBack
                  , headerEnd: Just $ searchBar search
                  , setTab: Just <<< SetTab
                  , tabs:
                    [ { name: PersonalProjects
                      , content: HH.text "Projects go here"
                      }
                    , { name: Examples
                      , content: HH.text "Examples go here"
                      }
                    , { name: Tutorials
                      , content: HH.text "Tutorials go here"
                      }
                    ]
                  }
              ]
      ]
