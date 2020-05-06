module Lunarbox.Data.Editor.Camera
  ( Camera(..)
  , toWorldCoordinates
  , toViewBox
  , zoomOrigin
  , zoomOn
  , origin
  , pan
  , screenPan
  , _CameraPosition
  , _CameraZoom
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default)
import Data.Lens (Lens', over, view)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Halogen.HTML (IProp)
import Lunarbox.Data.Editor.Constants (clampZoom)
import Lunarbox.Data.Lens (newtypeIso)
import Lunarbox.Data.Vector (Vec2)
import Math (floor)
import Svg.Attributes as SA

-- Holds information about the current viewbox in an easy to store format
newtype Camera
  = Camera
  { position :: Vec2 Number
  , zoom :: Number
  }

derive instance eqCamera :: Eq Camera

derive instance newtypeCamera :: Newtype Camera _

derive newtype instance encodeJsonCamera :: EncodeJson Camera

derive newtype instance decodeJsonCamera :: DecodeJson Camera

instance defaultCamera :: Default Camera where
  def =
    Camera
      { position: zero
      , zoom: 1.0
      }

-- Project a point on the screen into world coordinates
toWorldCoordinates :: Camera -> Vec2 Number -> Vec2 Number
toWorldCoordinates (Camera { position, zoom }) vec = position + ((_ / zoom) <$> vec)

-- Get the origin of the screen coordinates in world coordinates
origin :: Camera -> Vec2 Number
origin = view _CameraPosition

-- Zoom on the origin of the __screen__ view 
zoomOrigin :: Number -> Camera -> Camera
zoomOrigin = over _CameraZoom <<< (clampZoom <<< _) <<< (*)

-- Translate the camera using a vector in world coordinates
pan :: Vec2 Number -> Camera -> Camera
pan = over _CameraPosition <<< flip (-)

-- Translate the camera using a vector in screen coordinates
screenPan :: Vec2 Number -> Camera -> Camera
screenPan vector camera = pan worldCoordinates camera
  where
  worldCoordinates = toWorldCoordinates camera vector - origin camera

-- Zooms relative to a poin in screen coorinates
zoomOn :: Vec2 Number -> Number -> Camera -> Camera
zoomOn point amount = screenPan (point) <<< zoomOrigin amount <<< screenPan (-point)

-- Generate a svg viewbox from a Camera
toViewBox :: forall r i. Vec2 Number -> Camera -> IProp ( viewBox ∷ String | r ) i
toViewBox scale (Camera { position, zoom }) =
  SA.viewBox (floor $ position !! d0)
    (floor $ position !! d1)
    (floor $ scale !! d0 / zoom)
    (floor $ scale !! d1 / zoom)

-- Lenses
_CameraPosition :: Lens' Camera (Vec2 Number)
_CameraPosition = newtypeIso <<< prop (SProxy :: _ "position")

_CameraZoom :: Lens' Camera Number
_CameraZoom = newtypeIso <<< prop (SProxy :: _ "zoom")
