module Lunarbox.Data.ProjectList
  ( ProjectData
  , ProjectList
  , ProjectOverview
  , TutorialOverview
  , _userProjects
  , _exampleProjects
  ) where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.Tutorial (TutorialId)

type ProjectData r
  = ( name :: String
    , metadata ::
      { functionCount :: Int
      , nodeCount :: Int
      }
    | r
    )

-- | Data needed to render the projects in the list
type ProjectOverview
  = ProjectData ( id :: ProjectId )

-- | Data needed to render the tutorial in the list
type TutorialOverview
  = { name :: String
    , id :: TutorialId
    , completed :: Boolean
    , own :: Boolean
    }

-- | Stuff we get from the server to render on the projects page
type ProjectList
  = { exampleProjects :: Array { | ProjectOverview }
    , userProjects :: Array { | ProjectOverview }
    , tutorials :: Array TutorialOverview
    }

-- Lenses
_exampleProjects :: Lens' ProjectList (Array { | ProjectOverview })
_exampleProjects = prop (SProxy :: _ "exampleProjects")

_userProjects :: Lens' ProjectList (Array { | ProjectOverview })
_userProjects = prop (SProxy :: _ "userProjects")
