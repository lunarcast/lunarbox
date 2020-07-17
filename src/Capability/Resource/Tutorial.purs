module Lunarbox.Capability.Resource.Tutorial where

import Prelude
import Data.Either (Either)
import Halogen (HalogenM, lift)
import Lunarbox.Data.Tutorial (TutorialId, TutorialWithMetadata, Tutorial)

-- | Capability for managing tutorials
class
  Monad m <= ManageTutorials m where
  createTutorial :: m (Either String TutorialId)
  deleteTutorial :: TutorialId -> m (Either String Unit)
  saveTutorial :: TutorialId -> Tutorial () -> m (Either String Unit)
  getTutorial :: TutorialId -> m (Either String TutorialWithMetadata)

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance manageTutorialsHalogenM :: ManageTutorials m => ManageTutorials (HalogenM st act slots msg m) where
  createTutorial = lift createTutorial
  deleteTutorial = lift <<< deleteTutorial
  saveTutorial = (lift <<< _) <<< saveTutorial
  getTutorial = lift <<< getTutorial
