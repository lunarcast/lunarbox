module Lunarbox.Data.Math (normalizeAngle) where

import Prelude
import Math (Radians, tau, (%))

-- Take any number of radians and bring it in the [0, tau) interval
normalizeAngle :: Radians -> Radians
normalizeAngle angle =
  ( if angle < 0.0 then
      tau - (-angle % tau)
    else
      angle
  )
    % tau
