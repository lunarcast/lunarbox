module Lunarbox.Capability.Editor.Type
  ( typeToColor
  , generateColorMap
  , prettify
  , combineColors
  , inputNodeType
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Array (fold, foldMap, (!!))
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Filterable (filterMap)
import Data.Function (on)
import Data.Lens (view)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Dataflow.Class.Substituable (Substitution(..), apply)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type(..), typeBool, typeNumber, typeString)
import Lunarbox.Data.Dataflow.Type as Type
import Lunarbox.Data.Editor.Node (Node(..), _nodeInputs, hasOutput)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.NodeGroup (NodeGroup(..))
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

-- | Wrapper around Colors which can be combined and compared
-- TODO: when I get rid of the halogen-svg dependency 
-- I should find a better way to handle those colors
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
  shade = seededInt (show name) 50 220

-- Generate all possible pin locations for a certain node
pinLocations :: Node -> List.List Pin
pinLocations node = output <> inputs
  where
  inputs = List.mapWithIndex (\index -> const $ InputPin index) $ view _nodeInputs node

  output = OutputPin <$ (guard $ hasOutput node)

-- Createa a typeMap from a node and data about it
generateColorMap :: (Pin -> Maybe Type) -> Node -> Map.Map Pin Color
generateColorMap _ InputNode = mempty

generateColorMap getType node = Map.fromFoldable pairs
  where
  pairs =
    filterMap
      ( \pin ->
          Tuple pin
            <$> typeToColor
            <$> getType pin
      )
      $ pinLocations node

-- More complex version of generateColorMap specialised on input nodes
inputNodeType :: NodeGroup -> NodeId -> Type -> Map.Map Pin Type
inputNodeType (NodeGroup { nodes, inputs }) id ty = case List.index (Type.inputs ty) =<< inputIndex of
  Just ty' -> Map.singleton OutputPin ty'
  Nothing -> mempty
  where
  inputIndex = List.findIndex (id == _) inputs

-- getInputType :: NodeId -> 
-- Prettify variable names in types. Should only be used for pretty printing.
prettify :: Type -> Type
prettify type' = apply subst type'
  where
  -- Same as ftv but also returns non-generalizable variables
  variables :: Type -> List TVarName
  variables (TConstant _ vars) = foldMap variables vars

  variables (TVariable _ name) = pure name

  alphabet :: Array String
  alphabet = String.singleton <$> enumFromTo 'a' 'z'

  alphabetSize :: Int
  alphabetSize = Array.length alphabet

  free :: List TVarName
  free = variables type'

  freeCount :: Int
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
