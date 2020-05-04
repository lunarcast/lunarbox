module Lunarbox.Component.Projects
  ( component
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProjects)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Component.WithLogo (withLogo)
import Lunarbox.Config (Config)
import Lunarbox.Data.ProjectList (ProjectList, ProjectData)
import Network.RemoteData (RemoteData(..), fromEither)

type State
  = { projectList :: RemoteData String ProjectList
    }

-- Lenses
_projectList :: Lens' State (RemoteData String ProjectList)
_projectList = prop (SProxy :: _ "projectList")

data Action
  = Initialize

type Output
  = Void

type ChildSlots
  = ()

type Input
  = {}

component :: forall m q. MonadEffect m => ManageProjects m => MonadAsk Config m => Component HH.HTML q Input Output m
component =
  mkComponent
    { initialState: const { projectList: NotAsked }
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

  loading :: forall h a. HH.HTML h a
  loading = HH.text "loading"

  renderProject { name, functionCount, nodeCount } =
    HH.div [ className "project" ]
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

  listButton :: String -> HH.HTML _ _
  listButton name = HH.div [ className "list-button" ] [ icon name ]

  renderProjects { projectList } = case projectList of
    NotAsked -> pure loading
    Loading -> pure loading
    Failure err -> pure $ HH.text $ "error " <> err
    Success { projects, examples } ->
      [ renderProjectList "Projects" projects [ listButton "add" ]
      , renderProjectList "Examples" examples []
      ]

  render state =
    container "projects-container"
      [ withLogo
          $ container "projects"
              [ container "project-search"
                  [ container "search-spacing" []
                  , HH.input [ HP.placeholder "Search" ]
                  ]
              , container "project-lists"
                  $ renderProjects state
              ]
      ]
