module Lunarbox.Component.Utils
  ( OpaqueSlot
  , StaticHtml
  , className
  , container
  , busEventSource
  , whenElem
  , maybeElement
  ) where

import Prelude
import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe, maybe)
import Effect.Aff (forkAff, killFiber, error)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), Slot)
import Halogen.HTML (HTML, IProp)
import Halogen.HTML as HH
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

-- Conditional rendering helper
whenElem :: forall p i. Boolean -> (Unit -> HTML p i) -> HTML p i
whenElem condition f = if condition then f unit else HH.text ""

-- Unwrap some html from a maybe
maybeElement :: forall p i a. Maybe a -> (a -> HTML p i) -> HTML p i
maybeElement = flip $ maybe $ HH.text ""
