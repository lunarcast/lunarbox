module Lunarbox.Data.Editor.PartialConnection
  ( PartialConnection(..)
  , getSelectionStatus
  , _from
  , _to
  ) where

import Prelude
import Data.Default (class Default)
import Data.Lens (Lens', view)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Lunarbox.Component.Editor.Node (SelectionStatus(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Lens (newtypeIso)

-- This is a data structure used to store data about the current connection the user is working on
newtype PartialConnection
  = PartialConnection
  { from :: Maybe NodeId
  , to :: Maybe (Tuple NodeId Int)
  }

-- Typeclass instances
derive instance eqPartialConnection :: Eq PartialConnection

derive instance newtypePartialConnection :: Newtype PartialConnection _

derive newtype instance defaultPartialConnection :: Default PartialConnection

-- TTransform a PartialConnection into a SelectionStatus which can be understood by the Node component
getSelectionStatus :: PartialConnection -> NodeId -> SelectionStatus
getSelectionStatus partialConnection id =
  if view _from partialConnection == Just id then
    OutputSelected
  else case view _to partialConnection of
    Just (Tuple partialConnectionId index)
      | partialConnectionId == id -> InputSelected index
    _ -> NothingSelected

-- Lenses
_from :: Lens' PartialConnection (Maybe NodeId)
_from = newtypeIso <<< prop (SProxy :: _ "from")

_to :: Lens' PartialConnection (Maybe (Tuple NodeId Int))
_to = newtypeIso <<< prop (SProxy :: _ "to")
