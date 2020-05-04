module Lunarbox.Capability.Resource.Project
  ( class ManageProjects
  , getProjects
  ) where

import Prelude
import Data.Either (Either)
import Halogen (HalogenM, lift)
import Lunarbox.Data.ProjectList (ProjectList)

-- | This capability represents the ability to manage users in our system. We support logging users
-- | in, and registering them, as well as reading information about various users and who follows
-- | who.
-- |
-- | We'll handle all the mechanics of making the request, decoding responses, handling errors, and
-- | so on in the implementation.
class
  Monad m <= ManageProjects m where
  getProjects :: m (Either String ProjectList)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageProjects m => ManageProjects (HalogenM st act slots msg m) where
  getProjects = lift getProjects
