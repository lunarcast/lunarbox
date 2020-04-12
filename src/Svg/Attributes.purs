module Lunarbox.Svg.Attributes
  ( strokeWidth
  , arc
  , transparent
  ) where

import Prelude
import Data.Typelevel.Num (d0, d1)
import Data.Vec (vec2, (!!))
import Halogen.HTML (IProp)
import Lunarbox.Capability.Editor.Node.NodeInput (Arc(..), length)
import Lunarbox.Data.Vector (Vec2)
import Math (Radians, cos, pi, sin)
import Svg.Attributes (Color(..), Command(..))
import Svg.Attributes as SA
import Unsafe.Coerce (unsafeCoerce)

-- There's a bug with halogen-svg which doen's allow me to use this 
-- so I made a wrapper which allows me anywhere where I can use a stroke 
strokeWidth :: forall r i. Number -> IProp ( stroke :: String | r ) i
strokeWidth = unsafeCoerce SA.strokeWidth

polarToCartesian :: Number -> Radians -> Vec2 Number
polarToCartesian radius angle = (radius * _) <$> vec2 (cos angle) (sin angle)

arcLength :: Radians -> Radians -> Radians
arcLength start end = length $ Arc start end unit

-- The A command for svg is pretty hard to use. 
-- This is a wrapper which allows using it for circle arcs
arc :: Number -> Radians -> Radians -> Array Command
arc radius startAngle endAngle =
  let
    start = polarToCartesian radius endAngle

    end = polarToCartesian radius startAngle

    largeArcFlag = not $ arcLength startAngle endAngle >= pi
  in
    [ M (start !! d0) $ start !! d1, A radius radius 0.0 largeArcFlag true (end !! d0) (end !! d1) ]

-- Transparent color
transparent :: Color
transparent = RGBA 0 0 0 0.0
