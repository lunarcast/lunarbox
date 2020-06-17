module Lunarbox.Capability.Editor.Type
  ( typeToColor
  , generateTypeMap
  , prettify
  , combineColors
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (fold, foldMap, (!!))
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Function (on)
import Data.Lens (view)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNumber, typeString)
import Lunarbox.Data.Editor.FunctionData (FunctionData, _FunctionDataInputs)
import Lunarbox.Data.Editor.Node (Node, hasOutput)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Math.SeededRandom (seededInt)
import Lunarbox.Svg.Attributes (colorsAreEqual)
import Svg.Attributes (Color(..))

-- Calculates the averege of 2 ints
averege :: Int -> Int -> Int
averege a b = (a + b) / 2

-- Calculates the averege of 2 colors
combineColors :: Color -> Color -> Color
combineColors (RGBA r g b o) (RGBA r' g' b' o') = RGBA (averege r r') (averege g g') (averege b b') $ (o + o') / 2.0

combineColors (RGB r g b) color = combineColors (RGBA r g b 1.0) color

combineColors color (RGB r g b) = combineColors (RGBA r g b 1.0) color

-- Color for types we cannot paint by other means
unknownColor :: Color
unknownColor = RGB 52 235 222

-- Wrapper around Colors which can be combined and compared
newtype Color'
  = Color Color

derive instance newtypeColor' :: Newtype Color' _

instance eqColor' :: Eq Color' where
  eq = on colorsAreEqual unwrap

instance semigroupColor' :: Semigroup Color' where
  append a b
    | a == mempty = b
  append b a
    | a == mempty = b
  append (Color a) (Color b) = Color $ a `combineColors` b

instance monoidColor' :: Monoid Color' where
  mempty = Color unknownColor

-- Given a color returns a type
typeToColor :: Type -> Color
typeToColor t@(TConstant _ [])
  | t == typeString = RGB 97 196 35
  | t == typeBool = RGB 193 71 53
  | t == typeNumber = RGB 35 78 196
  | otherwise = unknownColor

typeToColor (TConstant _ vars) = unwrap $ fold $ Color <$> typeToColor <$> vars

typeToColor (TVariable _ name) = RGB shade shade shade
  where
  shade = seededInt (show name) 0 255

inputPins :: FunctionData -> List.List Pin
inputPins functionData =
  List.mapWithIndex (const <<< InputPin)
    $ Array.toUnfoldable
    $ view _FunctionDataInputs functionData

-- Generate all possible pin locations for a certain node
pinLocations :: FunctionData -> Node -> List.List Pin
pinLocations functionData node = (OutputPin <$ guard (hasOutput node)) <> inputPins functionData

-- Createa a typeMap from a node and data about it
generateTypeMap :: (Pin -> Maybe Type) -> FunctionData -> Node -> Map.Map Pin Color
generateTypeMap getType functionData node = Map.fromFoldable pairs
  where
  pairs =
    List.catMaybes
      $ ( \pin ->
            Tuple pin
              <$> typeToColor
              <$> getType pin
        )
      <$> pinLocations functionData node

alphabet :: Array String
alphabet = String.singleton <$> enumFromTo 'a' 'z'

alphabetSize :: Int
alphabetSize = Array.length alphabet

-- Same as ftv but also returns nont-generalizable variables
variables :: Type -> List TVarName
variables (TConstant "Function" [ from, to ]) = variables from <> variables to

variables (TConstant _ vars) = foldMap variables vars

variables (TVariable _ name) = pure name

-- Prettify variable names in types. Should only be used for pretty printing
prettify :: Type -> Type
prettify type' =
  let
    free = variables type'

    freeCount = List.length free

    subst =
      Substitution $ Map.fromFoldable
        $ List.mapWithIndex
            ( \index name ->
                Tuple name
                  $ TVariable true
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
