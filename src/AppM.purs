module Lunarbox.AppM where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as RF
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..), hush)
import Data.HTTP.Method as Method
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Lunarbox.Api.Endpoint (Endpoint(..))
import Lunarbox.Api.Request (RequestMethod(..), decodeJsonResponse)
import Lunarbox.Api.Request as Request
import Lunarbox.Api.Utils (authenticate, mkRawRequest, mkRequest, withBaseUrl)
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Gist (class ManageGists)
import Lunarbox.Capability.Resource.Project (class ManageProjects)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials)
import Lunarbox.Capability.Resource.User (class ManageUser)
import Lunarbox.Config (Config, _changeRoute, _currentUser, _userBus)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Editor.Save (jsonToState, stateToJson)
import Lunarbox.Data.Gist (GistId(..))
import Lunarbox.Data.ProjectId (ProjectId(..))
import Lunarbox.Data.ProjectList (ProjectOverview, TutorialOverview)
import Lunarbox.Data.Route (routingCodec)
import Lunarbox.Data.Route as Route
import Lunarbox.Data.Tutorial (TutorialId(..))
import Record as Record
import Routing.Duplex (print)

-- Todo: better type for errors
type Error
  = String

newtype AppM a
  = AppM (ReaderT Config Aff a)

runAppM :: forall a. Config -> AppM a -> Aff a
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadAskAppM :: MonadAsk Config AppM

derive newtype instance monadReaderAppM :: MonadReader Config AppM

instance navigateAppM :: Navigate AppM where
  navigate path = do
    changeRoute <- asks $ view _changeRoute
    liftEffect $ changeRoute (unsafeToForeign {}) $ print routingCodec path
  logout = do
    currentUser <- asks $ view _currentUser
    userBus <- asks $ view _userBus
    void $ mkRawRequest { endpoint: Logout, method: Get }
    liftEffect $ Ref.write Nothing currentUser
    liftAff $ Bus.write Nothing userBus
    navigate Route.Home

instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate Request.login
  registerUser = authenticate Request.register
  getCurrentUser = hush <$> withBaseUrl Request.profile

type ProjectIdData
  = { project :: { id :: ProjectId } }

instance manageProjectsAppM :: ManageProjects AppM where
  createProject state = do
    let
      body = stateToJson state
    response :: Either String ProjectIdData <- mkRequest { endpoint: Projects, method: Post $ Just $ encodeJson body }
    pure $ _.project.id <$> response
  cloneProject id = do
    response :: Either String ProjectIdData <- mkRequest { endpoint: Clone id, method: Get }
    pure $ _.project.id <$> response
  getProject id = do
    response <- mkRawRequest { endpoint: Project id, method: Get }
    pure $ jsonToState =<< response
  saveProject id json = void <$> mkRawRequest { endpoint: Project id, method: Put $ Just json }
  deleteProject id = void <$> mkRawRequest { endpoint: Project id, method: Delete }
  getProjects =
    -- All this mess is here to mock tutorials
    -- | TODO: Remove when bg finally updates the api
    map (Record.merge { tutorials: mockTutorials })
      <$> ( mkRequest { endpoint: Projects, method: Get } ::
            AppM
              ( Either String
                  { exampleProjects :: Array { | ProjectOverview }
                  , userProjects :: Array { | ProjectOverview }
                  }
              )
        )
    where
    mockTutorials :: Array TutorialOverview
    mockTutorials =
      [ { id: TutorialId 0
        , name: "A sample tutorial"
        , completed: false
        }
      , { id: TutorialId 1
        , name: "Another tutorial"
        , completed: false
        }
      , { id: TutorialId 2
        , name: "Actually completed this"
        , completed: true
        }
      ]

instance manageTutorialsAppM :: ManageTutorials AppM where
  createTutorial = pure $ Right $ TutorialId 0
  deleteTutorial id = Right <$> printString ("Deleted id " <> show id)
  saveTutorial id g = do
    printString $ "Saving project " <> show id
    pure $ Right unit
  getTutorial id
    -- We mock this until bg makes the api
    | id == TutorialId 7 =
      pure $ Right
        $ { name: "My super duper awesome tutorial"
          , base: ProjectId 74
          , solution: ProjectId 80
          , hiddenElements: []
          , id
          , content: GistId "c36e060c76f2493bed9df58285e3b13f"
          , completed: false
          }
    | otherwise = pure $ Left $ "Cannot find tutorial " <> show id

instance manageGistsAppM :: ManageGists AppM where
  fetchGist id = do
    result <-
      liftAff
        $ AX.request
        $ AX.defaultRequest
            { url = "https://api.github.com/gists/" <> show id
            , method = Left Method.GET
            , responseFormat = RF.json
            }
    pure $ decodeJsonResponse result >>= decodeJson
