module Lunarbox.Data.Editor.Node.NodeInput
  ( arcs
  ) where

import Prelude
import Data.Array as Array
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Lunarbox.Capability.Editor.Node.Arc (Arc, fillWith, normalize, rotate)
import Lunarbox.Data.Editor.FunctionData (FunctionData(..))
import Math (Radians, pi)

-- 90 degrees in radians
halfPi :: Radians
halfPi = pi / 2.0

-- Get a list of input arcs for any nodes
arcs :: FunctionData -> Tuple (List String) (List (Arc String))
arcs (FunctionData { inputs }) = Tuple inputNames $ rotate halfPi <$> normalize <$> fillWith inputNames Nil
  where
  inputNames = Array.toUnfoldable $ _.name <$> inputs
