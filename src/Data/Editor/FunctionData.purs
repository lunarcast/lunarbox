module Lunarbox.Data.Editor.FunctionData
  ( FunctionData(..)
  , PinDoc
  , getFunctionData
  , internal
  , outputData
  , _FunctionDataInputs
  , _FunctionDataOutput
  , _FunctionDataExternal
  , _PinName
  , _PinDescription
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', set)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Lens (newtypeIso)

type PinDoc
  = { name :: String
    , description :: String
    }

newtype FunctionData
  = FunctionData
  { external :: Boolean
  , inputs :: Array PinDoc
  , output :: PinDoc
  }

derive instance genericFunctionData :: Generic FunctionData _

derive instance newtypeFunctionData :: Newtype FunctionData _

derive newtype instance encodeJsonFunctionData :: EncodeJson FunctionData

derive newtype instance decodeJsonFunctionData :: DecodeJson FunctionData

instance showFunctionData :: Show FunctionData where
  show = genericShow

instance defaultFunctionData :: Default FunctionData where
  def =
    FunctionData
      { external: false
      , inputs: mempty
      , output: { name: "output", description: "the return value of a function" }
      }

-- Helpers
-- Function data for output nodes
outputData :: FunctionData
outputData = internal [ { name: "return value", description: "The return value of a function" } ] { name: "This node doesn't have an output", description: "" }

getFunctionData :: (FunctionName -> FunctionData) -> Node -> FunctionData
getFunctionData getter = case _ of
  ComplexNode { function } -> getter function
  -- TODO: find a good way to handle this
  OutputNode _ -> outputData
  InputNode -> def

-- Create data for an internal function
internal :: Array PinDoc -> PinDoc -> FunctionData
internal inputs output = set _FunctionDataOutput output $ set _FunctionDataInputs inputs def

-- Lenses
_FunctionDataExternal :: Lens' FunctionData Boolean
_FunctionDataExternal = newtypeIso <<< prop (SProxy :: _ "external")

_FunctionDataInputs :: Lens' FunctionData (Array PinDoc)
_FunctionDataInputs = newtypeIso <<< prop (SProxy :: _ "inputs")

_FunctionDataOutput :: Lens' FunctionData PinDoc
_FunctionDataOutput = newtypeIso <<< prop (SProxy :: _ "output")

_PinDescription :: Lens' PinDoc String
_PinDescription = prop (SProxy :: _ "description")

_PinName :: Lens' PinDoc String
_PinName = prop (SProxy :: _ "name")
