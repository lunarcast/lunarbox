module Lunarbox.Capability.Resource.Project
  ( class ManageProjects
  , getProjects
  , getProject
  , createProject
  , saveProject
  , saveRawProject
  ) where

import Prelude
import Data.Argonaut (Json)
import Data.Either (Either)
import Halogen (HalogenM, lift)
import Lunarbox.Data.Editor.State (State)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.ProjectList (ProjectList)

-- | Capability for managing projects
class
  Monad m <= ManageProjects m where
  getProjects :: m (Either String ProjectList)
  getProject :: forall a s m'. ProjectId -> m (Either String (State a s m'))
  createProject :: forall a s m'. State a s m' -> m (Either String ProjectId)
  saveProject :: forall a s m'. State a s m' -> m (Either String Unit)
  saveRawProject :: Json -> m (Either String Unit)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageUserHalogenM :: ManageProjects m => ManageProjects (HalogenM st act slots msg m) where
  getProjects = lift getProjects
  getProject = lift <<< getProject
  createProject = lift <<< createProject
  saveProject = lift <<< saveProject
  saveRawProject = lift <<< saveRawProject
