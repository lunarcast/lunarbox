module Lunarbox.Config where

import Prelude
import Control.Monad.Reader (class MonadAsk, class MonadReader, asks, local)
import Data.Lens (Lens', set)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Foreign (Foreign)
import Lunarbox.Api.Request (BaseUrl)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Profile (Profile)
import Routing.PushState (PushStateInterface, LocationState)

type DevOptions
  = Maybe
      -- It is usually useful to be able to not cancel  -- inputs when bluring off of them for debugging purpouses 
      -- I needed this so I could take screenshots without the input unfocusing itself
      { cancelInputsOnBlur :: Boolean
      }

type UserEnv
  = { currentUser :: Ref (Maybe Profile)
    , userBus :: BusRW (Maybe Profile)
    }

newtype Config
  = Config
  { devOptions :: DevOptions
  , baseUrl :: BaseUrl
  , user :: UserEnv
  , pushStateInterface :: PushStateInterface
  -- | Specifies what nodes are usable atm
  -- TODO: maybe make this not be global?
  , allowedNodes :: Maybe (Array FunctionName)
  }

derive instance newtypeConfig :: Newtype Config _

shouldCancelOnBlur :: forall m. Monad m => MonadAsk Config m => m Boolean
shouldCancelOnBlur = do
  { devOptions } <- asks unwrap
  case devOptions of
    Just { cancelInputsOnBlur } -> pure cancelInputsOnBlur
    Nothing -> pure true

-- | Run a monadic computation inside a context with a different base url
withBaseUrl :: forall m a. MonadReader Config m => BaseUrl -> m a -> m a
withBaseUrl = local <<< set _baseUrl

-- Lenses
_user :: Lens' Config UserEnv
_user = _Newtype <<< prop (SProxy :: _ "user")

_currentUser :: Lens' Config (Ref (Maybe Profile))
_currentUser = _user <<< prop (SProxy :: _ "currentUser")

_userBus :: Lens' Config (BusRW (Maybe Profile))
_userBus = _user <<< prop (SProxy :: _ "userBus")

_baseUrl :: Lens' Config BaseUrl
_baseUrl = _Newtype <<< prop (SProxy :: _ "baseUrl")

_allowedNodes :: Lens' Config (Maybe (Array FunctionName))
_allowedNodes = _Newtype <<< prop (SProxy :: _ "allowedNodes")

_pushStateInterface :: Lens' Config PushStateInterface
_pushStateInterface = _Newtype <<< prop (SProxy :: _ "pushStateInterface")

_changeRoute :: Lens' Config (Foreign -> String -> Effect Unit)
_changeRoute = _pushStateInterface <<< prop (SProxy :: _ "pushState")

_locationState :: Lens' Config (Effect LocationState)
_locationState = _pushStateInterface <<< prop (SProxy :: _ "locationState")
