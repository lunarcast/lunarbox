module Lunarbox.Config
  ( Config(..)
  , DevOptions(..)
  , UserEnv
  , shouldCancelOnBlur
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Lunarbox.Api.Requests (BaseUrl)
import Lunarbox.Data.Profile (Profile)

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
  }

derive instance newtypeConfig :: Newtype Config _

shouldCancelOnBlur :: forall m. Monad m => MonadAsk Config m => m Boolean
shouldCancelOnBlur = do
  { devOptions } <- asks unwrap
  case devOptions of
    Just { cancelInputsOnBlur } -> pure cancelInputsOnBlur
    Nothing -> pure true
