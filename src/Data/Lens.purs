module Lunarbox.Data.Lens where

import Data.Lens (Lens', iso)
import Data.Newtype (class Newtype, unwrap, wrap)

-- Generic iso which can be used for any data type with a newtype instance
newtypeIso :: forall a b. Newtype a b => Lens' a b
newtypeIso = iso unwrap wrap
