module Lunarbox.Capability.Resource.Gist where

import Prelude
import Data.Either (Either)
import Halogen (HalogenM, lift)
import Lunarbox.Data.Gist (Gist, GistId)

-- | Capability for managing gists
class
  Monad m <= ManageGists m where
  fetchGist :: GistId -> m (Either String Gist)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageGistsHalogenM :: ManageGists m => ManageGists (HalogenM st act slots msg m) where
  fetchGist = lift <<< fetchGist
