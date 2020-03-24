module Lunarbox.Data.FunctionData where

import Prelude
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Vec (vec2)
import Lunarbox.Data.Vector (Vec2)

type FunctionDataContent
  = { scale :: Vec2 Int, image :: String }

newtype FunctionData
  = FunctionData FunctionDataContent

derive instance newtypeFunctionData :: Newtype FunctionData _

instance semigroupFunctionData :: Semigroup FunctionData where
  append (FunctionData { scale, image }) (FunctionData { scale: scale' }) = FunctionData { scale: scale + scale', image }

instance monoidFunctionData :: Monoid FunctionData where
  mempty = FunctionData { scale: vec2 100 100, image: "https://static.zerochan.net/Okabe.Rintarou.full.762874.jpg" }

_scale :: Lens' FunctionDataContent (Vec2 Int)
_scale = prop (SProxy :: _ "scale")

_image :: Lens' FunctionDataContent String
_image = prop (SProxy :: _ "image")

_FunctionData :: Lens' FunctionData FunctionDataContent
_FunctionData = iso unwrap wrap

_FunctionDataScale :: Lens' FunctionData (Vec2 Int)
_FunctionDataScale = _FunctionData <<< _scale

_FunctionDataImage :: Lens' FunctionData String
_FunctionDataImage = _FunctionData <<< _image
