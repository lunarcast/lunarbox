module Lunarbox.Data.Ord (sortBySearch) where

import Prelude
import Data.Array as Array
import Data.Fuzzy (matchStr)
import Data.Tuple (Tuple(..), fst)

-- Order a list  by a search term
sortBySearch :: forall a. Ord a => (a -> String) -> String -> Array a -> Array a
sortBySearch toString search elements =
  if search == "" then
    elements
  else
    fst <$> sorted
  where
  withMatches = (\element -> Tuple element $ matchStr true search $ toString element) <$> elements

  sorted = Array.sortBy (\(Tuple _ match) (Tuple _ match') -> compare match match') withMatches
