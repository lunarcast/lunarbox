module Lunarbox.Api.Utils
  ( authenticate
  , mkRequest
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Lunarbox.Api.Request (BaseUrl)
import Lunarbox.Config (Config, _baseUrl, _user)
import Lunarbox.Control.Monad.Effect (print, printString)
import Lunarbox.Data.Profile (Profile)

-- Helper to make a request with the baseUrl from the reader monad
mkRequest ::
  forall m b.
  MonadAff m =>
  MonadAsk Config m =>
  (BaseUrl -> m (Either String b)) ->
  m (Maybe b)
mkRequest req = do
  baseUrl <- asks $ view _baseUrl
  response <- req baseUrl
  case response of
    Right result -> pure $ Just result
    Left error -> printString error *> pure Nothing

-- Helper to creating functions which request something which return a profile
authenticate ::
  forall m a.
  MonadAff m =>
  MonadAsk Config m =>
  (BaseUrl -> a -> m (Either String Profile)) ->
  a ->
  m (Either String Profile)
authenticate req fields = do
  { currentUser, userBus } <- asks $ view _user
  baseUrl <- asks $ view _baseUrl
  print "lol"
  req baseUrl fields
    >>= case _ of
        Left error -> printString error *> pure (Left error)
        Right profile -> do
          print profile
          liftEffect $ Ref.write (Just profile) currentUser
          --   any time we write to the current user ref, we should also broadcast the change
          liftAff $ Bus.write (Just profile) userBus
          pure (Right profile)
