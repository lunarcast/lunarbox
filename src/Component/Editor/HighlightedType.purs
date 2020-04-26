module Lunarbox.Component.Editor.HighlightedType
  ( highlightedType
  , highlightTypeToHTML
  , highlightTypeToSvg
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Lunarbox.Capability.Editor.Type (typeToColor)
import Lunarbox.Component.HighlightedText as HT
import Lunarbox.Data.Dataflow.Type (Type(..))
import Lunarbox.Math.SeededRandom (seededInt)
import Lunarbox.Svg.Element (tspan)
import Svg.Attributes (Color(..))
import Svg.Attributes as SA

-- A type which is syntax highlighted
highlightedType :: forall h a. (Array (HH.HTML h a) -> HH.HTML h a) -> (Color -> HH.HTML h a -> HH.HTML h a) -> Color -> Type -> HH.HTML h a
highlightedType container highlight defaultColor = case _ of
  TArrow from to ->
    container
      [ if isArrow then container [ HH.text "(", result, HH.text ")" ] else result
      , HH.text " -> "
      , highlightedType container highlight defaultColor to
      ]
    where
    isArrow = case from of
      TArrow _ _ -> true
      _ -> false

    result = highlightedType container highlight defaultColor from
  TVarariable name' -> highlight (RGB shade shade shade) $ HH.text $ show name'
    where
    shade = seededInt (show name') 100 255
  other -> highlight color $ HH.text $ show other
    where
    color = fromMaybe defaultColor $ typeToColor other

highlightTypeToHTML :: forall h a. Color -> Type -> HH.HTML h a
highlightTypeToHTML = highlightedType HH.span_ HT.highlight

highlightTypeToSvg :: forall h a. Color -> Type -> HH.HTML h a
highlightTypeToSvg =
  highlightedType (tspan []) \color ->
    tspan [ SA.fill $ Just color ] <<< pure
