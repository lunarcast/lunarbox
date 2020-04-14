module Lunarbox.Capability.Editor.Type
  ( typeToColor
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Dataflow.Type (Type(..), typeBool, typeNumber, typeString)
import Svg.Attributes (Color(..))

-- Calculates the averege of 2 ints
averege :: Int -> Int -> Int
averege a b = (a + b) / 2

-- Calculates the averege of 2 colors
combineColors :: Color -> Color -> Color
combineColors (RGBA r g b o) (RGBA r' g' b' o') = RGBA (averege r r') (averege g g') (averege b b') $ (o + o') / 2.0

combineColors (RGB r g b) color = combineColors (RGBA r g b 1.0) color

combineColors color (RGB r g b) = combineColors (RGBA r g b 1.0) color

-- Given a color returns a type
typeToColor :: Type -> Maybe Color
typeToColor (TArrow f t) = combineColors <$> typeToColor f <*> typeToColor t

typeToColor t
  | t == typeString = Just $ RGB 97 196 35
  | t == typeBool = Just $ RGB 193 71 53
  | t == typeNumber = Just $ RGB 35 78 196
  | otherwise = Nothing
