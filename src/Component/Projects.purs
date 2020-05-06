module Lunarbox.Component.Projects
  ( component
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..))
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, createProject, getProjects)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Component.WithLogo (withLogo)
import Lunarbox.Config (Config)
import Lunarbox.Data.Editor.State (emptyState)
import Lunarbox.Data.Ord (sortBySearch)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.ProjectList (ProjectList, ProjectData)
import Lunarbox.Data.Route (Route(..))
import Network.RemoteData (RemoteData(..), fromEither)

type State
  = { projectList :: RemoteData String ProjectList
    , search :: String
    }

-- Lenses
_projectList :: Lens' State (RemoteData String ProjectList)
_projectList = prop (SProxy :: _ "projectList")

_search :: Lens' State String
_search = prop (SProxy :: _ "search")

data Action
  = Initialize
  | OpenProject ProjectId
  | Search String
  | CreateProject

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
    OpenProject id -> navigate $ Project id
    CreateProject -> do
      response <- createProject emptyState
      -- Display a loading message
      modify_ $ set _projectList Loading
      case response of
        Right id -> navigate $ Project id
        Left err -> modify_ $ set _projectList $ Failure err

  loading :: forall h a. HH.HTML h a
  loading = HH.text "loading"

  renderProject { name, functionCount, nodeCount, id } =
    HH.div [ className "project", onClick $ const $ Just $ OpenProject id ]
      [ HH.div [ className "project-name" ] [ HH.text name ]
      , HH.div [ className "project-data" ]
          [ HH.div [ className "function-count" ]
              [ icon "functions"
              , HH.text $ show functionCount
              ]
          , HH.div [ className "node-count" ]
              [ icon "track_changes"
              , HH.text $ show nodeCount
              ]
          ]
      ]

  renderProjectList :: String -> Array ProjectData -> Array (HH.HTML _ _) -> HH.HTML _ _
  renderProjectList title projects buttons =
    container "project-list"
      [ container "list-header"
          $ [ container "list-title" [ HH.text title ]
            ]
          <> buttons
      , container
          "list-items"
          $ renderProject
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
      [ renderProjectList "Projects" (order userProjects) [ listButton "add" CreateProject ]
      , renderProjectList "Examples" (order exampleProjects) []
      ]
    where
    order = sortBySearch _.name search

  render state@{ search } =
    container "projects-container"
      [ withLogo
          $ container "projects"
              [ container "project-search"
                  [ container "search-spacing" []
                  , HH.input
                      [ HP.value search
                      , HP.placeholder "Search"
                      , onValueInput $ Just <<< Search
                      ]
                  ]
              , container "project-lists"
                  $ renderProjects state
              ]
      ]
