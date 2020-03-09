module Lunarbox.Component.Utils where

import Prelude
import Halogen (ClassName(..), Slot)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML.Elements as HH
import Halogen.HTML.Properties (class_, id_)

-- | When a component has no queries or messages, it has no public interface and can be
-- | considered an "opaque" component. The only way for a parent to interact with the
-- | component is by sending input.
type OpaqueSlot slot
  = forall query. Slot query Void slot

type StaticHtml a b c
  = a -> HTML b c

className :: forall r i. String -> IProp ( class ∷ String | r ) i
className = class_ <<< ClassName

container :: forall r i. String -> Array (HTML r i) -> HTML r i
container id content = HH.div [ id_ id ] content
