module Lunarbox.Data.FunctionData (FunctionData(..), _FunctionDataExternal, _FunctionDataImage, _FunctionDataScale) where

import Prelude
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Data.Vec (vec2)
import Lunarbox.Data.Vector (Vec2)

newtype FunctionData
  = FunctionData { scale :: Vec2 Int, image :: String, external :: Boolean }

derive instance newtypeFunctionData :: Newtype FunctionData _

instance semigroupFunctionData :: Semigroup FunctionData where
  append (FunctionData { scale, image, external }) (FunctionData { scale: scale', external: external' }) =
    FunctionData
      { scale: scale + scale'
      , image
      , external: external || external'
      }

instance monoidFunctionData :: Monoid FunctionData where
  mempty =
    FunctionData
      { scale: vec2 100 100
      , image: "https://static.zerochan.net/Okabe.Rintarou.full.762874.jpg"
      , external: false
      }

_FunctionData :: Lens' FunctionData { scale :: Vec2 Int, image :: String, external :: Boolean }
_FunctionData = iso unwrap wrap

_FunctionDataScale :: Lens' FunctionData (Vec2 Int)
_FunctionDataScale = _FunctionData <<< prop (SProxy :: _ "scale")

_FunctionDataImage :: Lens' FunctionData String
_FunctionDataImage = _FunctionData <<< prop (SProxy :: _ "image")

_FunctionDataExternal :: Lens' FunctionData Boolean
_FunctionDataExternal = _FunctionData <<< prop (SProxy :: _ "external")
