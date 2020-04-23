module Lunarbox.Data.Lens
  ( listToArrayIso
  , newtypeIso
  ) where

import Data.Array as Array
import Data.Lens (Lens', Iso', iso)
import Data.List as List
import Data.Newtype (class Newtype, unwrap, wrap)

-- Generic iso which can be used for any data type with a newtype instance
newtypeIso :: forall a b. Newtype a b => Lens' a b
newtypeIso = iso unwrap wrap

-- I usually use this when I want to focus on a single element of a lsit
listToArrayIso :: forall a. Iso' (List.List a) (Array a)
listToArrayIso = iso List.toUnfoldable Array.toUnfoldable
