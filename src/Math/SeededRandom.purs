module Lunarbox.Math.SeededRandom (seededInt) where

foreign import seededInt :: String -> Int -> Int -> Int
