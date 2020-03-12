module Lunarbox.Data.Project where

import Data.Map (Map)

newtype FunctionName
  = FunctionName String

type VisualFunction
  = Int

type Project
  = Map FunctionName VisualFunction
