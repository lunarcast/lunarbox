module Lunarbox.Data.Editor.PartialConnection
  ( PartialConnection(..)
  , _from
  , _to
  ) where

import Prelude
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple)
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

-- Lenses
_from :: Lens' PartialConnection (Maybe NodeId)
_from = newtypeIso <<< prop (SProxy :: _ "from")

_to :: Lens' PartialConnection (Maybe (Tuple NodeId Int))
_to = newtypeIso <<< prop (SProxy :: _ "to")
