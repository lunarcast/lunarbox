module Lunarbox.Data.ProjectList
  ( ProjectData
  , ProjectList
  , ProjectOverview
  , _userProjects
  , _exampleProjects
  ) where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
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

-- Lenses
_exampleProjects :: Lens' ProjectList (Array { | ProjectOverview })
_exampleProjects = prop (SProxy :: _ "exampleProjects")

_userProjects :: Lens' ProjectList (Array { | ProjectOverview })
_userProjects = prop (SProxy :: _ "userProjects")
