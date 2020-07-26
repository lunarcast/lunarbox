module Lunarbox.Data.MouseButton
  ( MouseButton(..)
  , buttonCode
  , isPressed
  ) where

import Prelude
import Data.Int.Bits (and)

-- ADT with all the mouse buttons (well, those are not all of them but I don't need the other ones)
data MouseButton
  = LeftButton
  | Wheel
  | RightButton

-- Turns a button into it's code
buttonCode :: MouseButton -> Int
buttonCode = case _ of
  LeftButton -> 1
  Wheel -> -1 -- Todo: find the correct code for this
  RightButton -> 2

-- Check if a button is pressed
isPressed :: MouseButton -> Int -> Boolean
isPressed button bits = 0 /= bits `and` buttonCode button
