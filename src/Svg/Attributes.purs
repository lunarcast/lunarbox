module Lunarbox.Svg.Attributes
  ( strokeWidth
  , fontWeight
  , arc
  , chord
  , transparent
  , strokeDashArray
  , strokeLinecap
  , Linecap(..)
  ) where

import Prelude
import Core (attr)
import Data.String (joinWith)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (AttrName(..), IProp)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..), length)
import Lunarbox.Data.Math (polarToCartesian)
import Math (Radians, pi)
import Svg.Attributes (Color(..), Command(..))
import Svg.Attributes as SA
import Unsafe.Coerce (unsafeCoerce)

-- There's a bug with halogen-svg which doen's allow me to use this 
-- so I made a wrapper which allows me anywhere where I can use a stroke 
strokeWidth :: forall r i. Number -> IProp ( stroke :: String | r ) i
strokeWidth = unsafeCoerce SA.strokeWidth

-- The halogen-svg lib doesn't support this so I had to make my own
strokeDashArray :: forall r i. Array Number -> IProp ( stroke :: String | r ) i
strokeDashArray = unsafeCoerce $ attr (AttrName "stroke-dasharray") <<< joinWith ","

-- The lib doesn't have this one either...
fontWeight :: forall r i. String -> IProp r i
fontWeight = unsafeCoerce $ attr (AttrName "font-weight")

-- stroke linecaps for svg
data Linecap
  = Butt
  | Round
  | Square

instance showLinecap :: Show Linecap where
  show Butt = "butt"
  show Round = "round"
  show Square = "square"

-- Same reason I have this as strokeDashArray
strokeLinecap :: forall r i. Linecap -> IProp ( stroke :: String | r ) i
strokeLinecap = unsafeCoerce <<< attr (AttrName "stroke-linecap") <<< show

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
    [ M (start !! d0) $ start !! d1
    , A radius
        radius
        0.0
        largeArcFlag
        true
        (end !! d0)
        (end !! d1)
    ]

-- Like an arc but goes directly to the target
chord :: Number -> Radians -> Radians -> Array Command
chord radius startAngle endAngle =
  let
    start = polarToCartesian radius endAngle

    end = polarToCartesian radius startAngle
  in
    [ M (start !! d0) $ start !! d1, L (end !! d0) (end !! d1) ]

-- Transparent color
transparent :: Color
transparent = RGBA 0 0 0 0.0
