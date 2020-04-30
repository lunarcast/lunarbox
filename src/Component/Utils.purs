module Lunarbox.Component.Utils
  ( OpaqueSlot
  , StaticHtml
  , className
  , container
  , busEventSource
  ) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Effect.Aff (forkAff, killFiber, error)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Slot)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML.Elements as HH
import Halogen.HTML.Properties (class_, id_)
import Halogen.Query.EventSource as ES

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

-- Create an event source for the global bsu
busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
