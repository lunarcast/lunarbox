module Lunarbox.Config
  ( Config(..)
  , DevOptions(..)
  , UserEnv
  , shouldCancelOnBlur
  , _user
  , _currentUser
  , _baseUrl
  , _locationState
  , _pushStateInterface
  , _changeRoute
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Foreign (Foreign)
import Lunarbox.Api.Request (BaseUrl)
import Lunarbox.Data.Lens (newtypeIso)
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
  }

derive instance newtypeConfig :: Newtype Config _

shouldCancelOnBlur :: forall m. Monad m => MonadAsk Config m => m Boolean
shouldCancelOnBlur = do
  { devOptions } <- asks unwrap
  case devOptions of
    Just { cancelInputsOnBlur } -> pure cancelInputsOnBlur
    Nothing -> pure true

-- Lenses
_user :: Lens' Config UserEnv
_user = newtypeIso <<< prop (SProxy :: _ "user")

_currentUser :: Lens' Config (Ref (Maybe Profile))
_currentUser = _user <<< prop (SProxy :: _ "currentUser")

_baseUrl :: Lens' Config BaseUrl
_baseUrl = newtypeIso <<< prop (SProxy :: _ "baseUrl")

_pushStateInterface :: Lens' Config PushStateInterface
_pushStateInterface = newtypeIso <<< prop (SProxy :: _ "pushStateInterface")

_changeRoute :: Lens' Config (Foreign -> String -> Effect Unit)
_changeRoute = _pushStateInterface <<< prop (SProxy :: _ "pushState")

_locationState :: Lens' Config (Effect LocationState)
_locationState = _pushStateInterface <<< prop (SProxy :: _ "locationState")
