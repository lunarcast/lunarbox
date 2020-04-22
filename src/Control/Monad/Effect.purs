module Lunarbox.Control.Monad.Effect (print, printString) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)

-- pirnt anything with a Show instance to the console
print :: forall m s. MonadEffect m => Show s => s -> m Unit
print = liftEffect <<< logShow

-- print a string to the console
printString :: forall m. MonadEffect m => String -> m Unit
printString = liftEffect <<< log
