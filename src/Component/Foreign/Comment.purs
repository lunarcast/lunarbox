module Lunarbox.Component.Foreign.Comment (autoExpand) where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Lunarbox.Data.Editor.Node.CommentData (minHeight)
import Web.HTML (HTMLTextAreaElement)

foreign import autoExpandImpl :: Number -> HTMLTextAreaElement -> Effect Number

-- Expand a textarea to match it's content and return the new height
autoExpand :: forall m. MonadEffect m => HTMLTextAreaElement -> m Number
autoExpand = liftEffect <<< autoExpandImpl minHeight
