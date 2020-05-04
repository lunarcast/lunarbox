module Lunarbox.Data.ProjectList
  ( ProjectData
  , ProjectList
  ) where

import Lunarbox.Data.ProjectId (ProjectId)

type ProjectData
  = { name :: String
    , id :: ProjectId
    , functionCount :: Int
    , nodeCount :: Int
    }

type ProjectList
  = { examples :: Array ProjectData
    , projects :: Array ProjectData
    }