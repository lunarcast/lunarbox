module Lunarbox.Data.Math
  ( normalizeAngle
  , polarToCartesian
  ) where

import Prelude
import Data.Vec (vec2)
import Lunarbox.Data.Vector (Vec2)
import Math (Radians, cos, sin, tau, (%))

-- Take any number of radians and bring it in the [0, tau) interval
normalizeAngle :: Radians -> Radians
normalizeAngle angle =
  ( if angle < 0.0 then
      tau - (-angle % tau)
    else
      angle
  )
    % tau

-- Transforms from polar coordinates to cartesian coordinates
polarToCartesian :: Number -> Radians -> Vec2 Number
polarToCartesian radius angle = (radius * _) <$> vec2 (cos angle) (sin angle)
