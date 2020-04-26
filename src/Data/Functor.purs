module Lunarbox.Data.Functor (indexed) where

import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Tuple (Tuple(..))

-- Asspcoiate all the elements of an indexed functor with an index
-- Example: 
--  Given ["a", "b", "c]
--  this returns [(0, "a"), (1, "b"), (2, "C")]
indexed :: forall a i f. FunctorWithIndex i f => f a -> f (Tuple i a)
indexed = mapWithIndex Tuple
