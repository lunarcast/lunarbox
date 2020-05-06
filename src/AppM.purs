module Lunarbox.AppM where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, asks, runReaderT)
import Data.Argonaut (stringify)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (unsafeToForeign)
import Lunarbox.Api.Endpoint (Endpoint(..))
import Lunarbox.Api.Request (RequestMethod(..))
import Lunarbox.Api.Request as Request
import Lunarbox.Api.Utils (authenticate, mkRequest, withBaseUrl)
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects)
import Lunarbox.Capability.Resource.User (class ManageUser)
import Lunarbox.Config (Config, _changeRoute)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Editor.Save (stateToJson)
import Lunarbox.Data.Editor.State (emptyState)
import Lunarbox.Data.ProjectId (ProjectId(..))
import Lunarbox.Data.Route (routingCodec)
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
  logout = void $ (mkRequest { endpoint: Logout, method: Post Nothing } :: AppM (_ {}))

instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate Request.login
  registerUser = authenticate Request.register
  getCurrentUser = withBaseUrl Request.profile

instance manageProjectsAppM :: ManageProjects AppM where
  createProject state = pure $ Right $ ProjectId "mock"
  getProject id = pure $ Right emptyState
  saveProject state = do
    printString $ "Saving " <> stringify (stateToJson state)
    pure $ Right unit
  getProjects =
    pure
      $ Right
          { examples:
            [ { id: ProjectId "1"
              , name: "test project"
              , functionCount: 2
              , nodeCount: 23
              }
            , { id: ProjectId "2"
              , name: "test project 2"
              , functionCount: 12
              , nodeCount: 235
              }
            , { id: ProjectId "3"
              , name: "test project 3"
              , functionCount: 5
              , nodeCount: 22
              }
            ]
          , projects:
            [ { id: ProjectId "1"
              , name: "test project"
              , functionCount: 2
              , nodeCount: 23
              }
            , { id: ProjectId "2"
              , name: "test project 2"
              , functionCount: 12
              , nodeCount: 235
              }
            , { id: ProjectId "3"
              , name: "test project 3"
              , functionCount: 5
              , nodeCount: 22
              }
            ]
          }
