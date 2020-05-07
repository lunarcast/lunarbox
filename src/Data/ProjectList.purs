module Lunarbox.Data.ProjectList
  ( ProjectData
  , ProjectList
  , ProjectOverview
  ) where

import Lunarbox.Data.ProjectId (ProjectId)

type ProjectData r
  = ( name :: String
    , metadata ::
      { functionCount :: Int
      , nodeCount :: Int
      }
    | r
    )

type ProjectOverview
  = ProjectData ( id :: ProjectId )

type ProjectList
  = { exampleProjects :: Array { | ProjectOverview }
    , userProjects :: Array { | ProjectOverview }
    }
