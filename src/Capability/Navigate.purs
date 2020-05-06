module Lunarbox.Capability.Navigate where

import Prelude
import Control.Monad.Trans.Class (lift)
import Lunarbox.Data.Route (Route)
import Halogen (HalogenM)

class
  Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout
