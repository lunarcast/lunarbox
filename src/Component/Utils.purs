module Lunarbox.Component.Utils where

import Prelude
import Halogen (Slot)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot
  = forall query. Slot query Void slot
