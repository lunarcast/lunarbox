module Data.Editor.Foreign.SceneBoundingBox (getSceneBoundingBox) where

import Effect (Effect)
import Web.HTML.HTMLElement (DOMRect)

foreign import getSceneBoundingBox :: Effect DOMRect
