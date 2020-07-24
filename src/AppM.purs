module Lunarbox.AppM where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as RF
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array as Array
import Data.Default (def)
import Data.Either (Either(..), hush)
import Data.Functor (voidRight)
import Data.HTTP.Method as Method
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (for)
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
import Lunarbox.Config (Config, _allowedNodes, _changeRoute, _currentUser, _userBus)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Dataflow.Native.NativeConfig (NativeConfig(..), loadNativeConfigs)
import Lunarbox.Data.Dataflow.Native.Prelude as Prelude
import Lunarbox.Data.Editor.Save (jsonToState, stateToJson)
import Lunarbox.Data.Editor.State (compile)
import Lunarbox.Data.Gist (GistId)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.ProjectList (ProjectOverview, TutorialOverview)
import Lunarbox.Data.Route (routingCodec)
import Lunarbox.Data.Route as Route
import Lunarbox.Data.Tutorial (TutorialId)
import Lunarbox.Data.Tutorial as Tutorial
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
    for (response >>= jsonToState) \project -> do
      allowed <- asks $ view _allowedNodes
      pure
        $ compile case allowed of
            Just nodes ->
              loadNativeConfigs
                (Array.filter go Prelude.configs)
                project
              where
              go (NativeConfig { name }) = Set.member name nodeSet

              nodeSet = Set.fromFoldable nodes
            Nothing -> Prelude.loadPrelude project
  saveProject id json = void <$> mkRawRequest { endpoint: Project id, method: Put $ Just json }
  deleteProject id = void <$> mkRawRequest { endpoint: Project id, method: Delete }
  getProjects =
    -- All this mess is here to mock tutorials
    -- | TODO: Remove when bg finally updates the api
    map (Record.rename (SProxy :: _ "tutorialProjects") (SProxy :: _ "tutorials"))
      <$> ( mkRequest { endpoint: Projects, method: Get } ::
            AppM
              ( Either String
                  { exampleProjects :: Array { | ProjectOverview }
                  , userProjects :: Array { | ProjectOverview }
                  , tutorialProjects :: Array TutorialOverview
                  }
              )
        )

instance manageTutorialsAppM :: ManageTutorials AppM where
  createTutorial projectData = do
    response :: Either String { tutorial :: { id :: TutorialId } } <-
      mkRequest { endpoint: Tutorials, method: Post $ Just $ encodeJson tutorial }
    pure $ _.id <$> _.tutorial <$> response
    where
    tutorial :: Tutorial.Tutorial
    tutorial =
      Record.merge
        { content: def :: GistId
        , name: "My tutorial"
        }
        projectData
  deleteTutorial id = mkRequest { endpoint: Tutorial id, method: Delete }
  completeTutorial id = do
    printString $ "Completed tutorial " <> show id
    pure $ Right unit
  saveTutorial id g =
    (voidRight unit)
      <$> mkRawRequest
          { endpoint: Tutorial id
          , method: Put $ Just $ encodeJson g
          }
  getTutorial id = mkRequest { endpoint: Tutorial id, method: Get }

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
