module Lunarbox.AppM where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Lunarbox.Capability.Navigate (class Navigate)
import Lunarbox.Config (Config)
import Lunarbox.Data.Route (routingCodec)
import Routing.Duplex (print)
import Routing.Hash (setHash)

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

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routingCodec
