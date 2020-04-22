module Lunarbox.Capability.Editor.Type
  ( ColoringError(..)
  , typeToColor
  , generateTypeMap
  , prettify
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Enum (enumFromTo)
import Data.Lens (view)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String.CodeUnits as String
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply, ftv)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNumber, typeString)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs)
import Lunarbox.Data.Editor.Location (Location)
import Lunarbox.Data.Editor.Node (Node, hasOutput)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Math.SeededRandom (seededInt)
import Svg.Attributes (Color(..))

-- Calculates the averege of 2 ints
averege :: Int -> Int -> Int
averege a b = (a + b) / 2

-- Calculates the averege of 2 colors
combineColors :: Color -> Color -> Color
combineColors (RGBA r g b o) (RGBA r' g' b' o') = RGBA (averege r r') (averege g g') (averege b b') $ (o + o') / 2.0

combineColors (RGB r g b) color = combineColors (RGBA r g b 1.0) color

combineColors color (RGB r g b) = combineColors (RGBA r g b 1.0) color

-- Given a color returns a type
typeToColor :: Type -> Maybe Color
typeToColor (TArrow f t) = combineColors <$> typeToColor f <*> typeToColor t

typeToColor t
  | t == typeString = Just $ RGB 97 196 35
  | t == typeBool = Just $ RGB 193 71 53
  | t == typeNumber = Just $ RGB 35 78 196
  | otherwise = Nothing

-- Errors which might occur while generating a typemap
data ColoringError
  = UnableToColor Type
  | MissingType Location

instance showColoringError :: Show ColoringError where
  show (UnableToColor type') = "Unable to generate a color for type " <> show type'
  show (MissingType location) = "Cannot find type for " <> show location

alwaysInput :: forall a. Int -> a -> Pin
alwaysInput = const <<< InputPin

inputPins :: FunctionData -> List.List Pin
inputPins functionData =
  List.mapWithIndex alwaysInput
    $ Array.toUnfoldable
    $ view _FunctionDataInputs functionData

-- Generate all possible pin locations for a certain node
pinLocations :: FunctionData -> Node -> List.List Pin
pinLocations functionData node = (OutputPin <$ guard (hasOutput node)) <> inputPins functionData

-- Create a location-color pair from a node and data related to it
generateColorPair :: Pin -> Type -> Either ColoringError (Tuple Pin Color)
generateColorPair currentLocation pinType = do
  color <- case pinType of
    TVarariable name' -> Right $ RGB shade shade shade
      where
      a =
        if currentLocation == InputPin 0 then
          trace name' identity
        else
          unit

      shade = seededInt (show name') 100 255
    other -> note (UnableToColor other) $ typeToColor other
  pure $ Tuple currentLocation color

-- Createa a typeMap from a node and data about it
generateTypeMap :: (Pin -> Maybe Type) -> FunctionData -> Node -> Either ColoringError (Map.Map Pin Color)
generateTypeMap getType functionData node = Map.fromFoldable <$> pairs
  where
  pairs =
    ( sequence
        $ List.catMaybes
        $ (\pin -> generateColorPair pin <$> getType pin)
        <$> pinLocations functionData node
    )

alphabet :: Array String
alphabet = String.singleton <$> enumFromTo 'a' 'z'

alphabetSize :: Int
alphabetSize = Array.length alphabet

-- Prettify variable names in types. Should only be used for pretty printing
prettify :: Type -> Type
prettify type' =
  let
    free = Set.toUnfoldable $ ftv type'

    freeCount = Array.length free

    subst =
      Substitution $ Map.fromFoldable
        $ Array.mapWithIndex
            ( \index name ->
                Tuple name
                  $ TVarariable
                      let
                        char =
                          TVarName
                            $ fromMaybe (show name)
                            $ alphabet
                            !! index `mod` alphabetSize
                      in
                        if index < alphabetSize then
                          char
                        else
                          char <> (TVarName $ show $ index / alphabetSize)
            )
            free
  in
    apply subst type'
