module Lunarbox.Data.Char (arrow) where

import Prelude
import Data.Char (fromCharCode)
import Data.Maybe (maybe)
import Data.String.CodeUnits as Char

-- Used for printing the types
arrow :: String
arrow = maybe "->" Char.singleton $ fromCharCode 8594
