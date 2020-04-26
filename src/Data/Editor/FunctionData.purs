module Lunarbox.Data.Editor.FunctionData
  ( FunctionData(..)
  , getFunctionData
  , internal
  , outputData
  , _FunctionDataExternal
  , _FunctionDataInputs
  ) where

import Prelude
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', iso, set)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node (Node(..))

newtype FunctionData
  = FunctionData
  { external :: Boolean
  , inputs ::
    Array
      { name :: String
      }
  }

derive instance genericFunctionData :: Generic FunctionData _

derive instance newtypeFunctionData :: Newtype FunctionData _

instance showFunctionData :: Show FunctionData where
  show = genericShow

instance defaultFunctionData :: Default FunctionData where
  def =
    FunctionData
      { external: false
      , inputs: mempty
      }

-- Helpers
-- Function data for output nodes
outputData :: FunctionData
outputData = internal [ { name: "return value" } ]

getFunctionData :: (FunctionName -> FunctionData) -> Node -> FunctionData
getFunctionData getter = case _ of
  ComplexNode { function } -> getter function
  -- TODO: find a good way to handle this
  OutputNode _ -> outputData
  InputNode -> def

-- Create data for an internal function
internal :: Array { name :: String } -> FunctionData
internal = flip (set _FunctionDataInputs) def

-- Lenses
_FunctionData ::
  Lens' FunctionData
    { external :: Boolean
    , inputs ::
      Array
        { name :: String
        }
    }
_FunctionData = iso unwrap wrap

_FunctionDataExternal :: Lens' FunctionData Boolean
_FunctionDataExternal = _FunctionData <<< prop (SProxy :: _ "external")

_FunctionDataInputs ::
  Lens' FunctionData
    ( Array
        { name :: String
        }
    )
_FunctionDataInputs = _FunctionData <<< prop (SProxy :: _ "inputs")
