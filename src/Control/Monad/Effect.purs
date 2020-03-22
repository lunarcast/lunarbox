module Lunarbox.Control.Monad.Effect where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (logShow)

print :: forall m s. MonadEffect m => Show s => s -> m Unit
print = liftEffect <<< logShow
