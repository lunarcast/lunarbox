module Lunarbox.Data.Editor.Location
  ( Location(..)
  , _UnknownLocation
  , _Function
  , _ScopedLocation
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.PinLocation (ScopedLocation(..))
import Lunarbox.Data.String (doubleShow)

-- Location for stuff in Projects
data Location
  = AtFunction FunctionName
  | InsideFunction FunctionName ScopedLocation
  | AtFunctionDeclaration FunctionName
  | FixpointOperator FunctionName
  | UnknownLocation

-- Lenses
_UnknownLocation :: Prism' Location Unit
_UnknownLocation =
  prism' (const UnknownLocation) case _ of
    UnknownLocation -> Just unit
    _ -> Nothing

_Function :: Lens' Location (Maybe FunctionName)
_Function =
  lens
    ( case _ of
        AtFunction name -> Just name
        InsideFunction name _ -> Just name
        AtFunctionDeclaration name -> Just name
        FixpointOperator name -> Just name
        _ -> Nothing
    )
    ( \function maybeName -> case maybeName of
        Just name -> case function of
          UnknownLocation -> UnknownLocation
          InsideFunction _ next -> InsideFunction name next
          AtFunction _ -> AtFunction name
          AtFunctionDeclaration _ -> AtFunctionDeclaration name
          FixpointOperator _ -> FixpointOperator name
        Nothing -> function
    )

_ScopedLocation :: Lens' Location (Maybe ScopedLocation)
_ScopedLocation =
  lens
    ( case _ of
        InsideFunction _ location -> Just location
        _ -> Nothing
    )
    ( \other -> case other of
        InsideFunction name _ -> case _ of
          Just location -> InsideFunction name location
          Nothing -> other
        _ -> const other
    )

-- Typeclass instances
derive instance eqLocation :: Eq Location

derive instance ordLocation :: Ord Location

derive instance genericLocation :: Generic Location _

instance encodeJsonLocation :: EncodeJson Location where
  encodeJson = genericEncodeJson

instance decodeJsonLocation :: DecodeJson Location where
  decodeJson = genericDecodeJson

instance defaultLocation :: Default Location where
  def = UnknownLocation

instance showLocation :: Show Location where
  show (AtFunction name) = "inside function" <> doubleShow name
  show (InsideFunction name FunctionDeclaration) = "at the declaration of function " <> doubleShow name
  show (InsideFunction name (NodeDefinition id)) = "at node " <> doubleShow id <> " in function " <> doubleShow name
  show (InsideFunction name location) = show location <> " in function " <> doubleShow name
  show UnknownLocation = "at an unknown location"
  show (AtFunctionDeclaration name) = "at the declaration of function " <> doubleShow name
  show (FixpointOperator name) = "at the recursion handler for " <> doubleShow name
