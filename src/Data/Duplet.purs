module Lunarbox.Data.Duplet (Duplet(..)) where

import Prelude

data Duplet a
  = Duplet a a

derive instance eqDuplet :: Eq a => Eq (Duplet a)

derive instance ordDuplet :: Ord a => Ord (Duplet a)
