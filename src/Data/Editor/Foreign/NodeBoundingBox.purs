module Lunarbox.Data.Editor.Foreign.NodeBoundingdBox where

import Web.DOM (Node)
import Web.HTML.HTMLElement (DOMRect)

foreign import nodeBoundingBox :: Node -> DOMRect
