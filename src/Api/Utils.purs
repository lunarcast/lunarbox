module Lunarbox.Api.Utils
  ( authenticate
  , mkRequest
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..), hush)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Lunarbox.Api.Request (BaseUrl)
import Lunarbox.Config (Config, _baseUrl, _user)
import Lunarbox.Data.Profile (Profile)

-- Helper to make a request with the baseUrl from the reader monad
mkRequest ::
  forall m b e.
  MonadAff m =>
  MonadAsk Config m =>
  (BaseUrl -> m (Either e b)) ->
  m (Maybe b)
mkRequest req = do
  baseUrl <- asks $ view _baseUrl
  response <- req baseUrl
  pure $ hush response

-- Helper to creating functions which request something which return a profile
authenticate ::
  forall m a.
  MonadAff m =>
  MonadAsk Config m =>
  (BaseUrl -> a -> m (Either String Profile)) ->
  a ->
  m (Maybe Profile)
authenticate req fields = do
  { currentUser, userBus } <- asks $ view _user
  baseUrl <- asks $ view _baseUrl
  req baseUrl fields
    >>= case _ of
        Left err -> pure Nothing
        Right profile -> do
          liftEffect $ Ref.write (Just profile) currentUser
          --   any time we write to the current user ref, we should also broadcast the change
          liftAff $ Bus.write (Just profile) userBus
          pure (Just profile)
