module Lunarbox.Data.Editor.Camera
  ( Camera(..)
  , toWorldCoordinates
  , toViewBox
  ) where

import Prelude
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (IProp)
import Lunarbox.Data.Vector (Vec2)
import Svg.Attributes as SA

-- Holds information about the current viewbox in an easy to store format
newtype Camera
  = Camera
  { position :: Vec2 Number
  , scale :: Vec2 Number
  , zoom :: Number
  }

-- Project a point on the screen into world coordinates
toWorldCoordinates :: Camera -> Vec2 Number -> Vec2 Number
toWorldCoordinates (Camera { position, zoom }) vec = position + ((_ / zoom) <$> vec)

-- Generate a svg viewbox from a Camera
toViewBox :: forall r i. Camera -> IProp ( viewBox ∷ String | r ) i
toViewBox (Camera { position, zoom, scale }) = SA.viewBox (position !! d0) (position !! d1) (scale !! d0 * zoom) (scale !! d1 * zoom)
