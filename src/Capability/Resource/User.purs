module Lunarbox.Capability.Resource.User
  ( class ManageUser
  , loginUser
  , registerUser
  , getCurrentUser
  ) where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Lunarbox.Api.Request (LoginFields, RegisterFields)
import Lunarbox.Data.Profile (Profile)

-- This capability represents the ability to manage users in our system.
class
  Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Either String Profile)
  registerUser :: RegisterFields -> m (Either String Profile)
  getCurrentUser :: m (Maybe Profile)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
  loginUser = lift <<< loginUser
  registerUser = lift <<< registerUser
  getCurrentUser = lift getCurrentUser
