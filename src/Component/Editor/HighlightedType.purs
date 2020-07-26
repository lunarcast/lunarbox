module Lunarbox.Component.Editor.HighlightedType
  ( highlightedType
  , highlightTypeToHTML
  ) where

import Prelude
import Color (Color, rgb)
import Halogen.HTML as HH
import Lunarbox.Capability.Editor.Type (typeToColor)
import Lunarbox.Component.HighlightedText (bold)
import Lunarbox.Component.HighlightedText as HT
import Lunarbox.Data.Char (arrow)
import Lunarbox.Data.Dataflow.Type (Type(..))
import Lunarbox.Data.String (spaced)
import Lunarbox.Math.SeededRandom (seededInt)

-- A type which is syntax highlighted
highlightedType ::
  forall h a.
  (Array (HH.HTML h a) -> HH.HTML h a) ->
  (HH.HTML h a -> HH.HTML h a) ->
  (Color -> HH.HTML h a -> HH.HTML h a) ->
  Type -> HH.HTML h a
highlightedType container bold highlight =
  let
    -- We need to take a type apram instead of just using partail apliaction
    -- to prevent infinite recursion
    continue type' = highlightedType container bold highlight type'
  in
    case _ of
      TConstant "Function" [ from, to ] ->
        container
          [ if isArrow then container [ HH.text "(", result, HH.text ")" ] else result
          , bold $ HH.text $ spaced arrow
          , continue to
          ]
        where
        isArrow = case from of
          TConstant "Function" [ _, _ ] -> true
          _ -> false

        result = continue from
      TConstant "Array" [ inner ] ->
        container
          [ bold $ HH.text "["
          , continue inner
          , bold $ HH.text "]"
          ]
      TVariable _ name' -> highlight (rgb shade shade shade) $ HH.text $ show name'
        where
        shade = seededInt (show name') 100 255
      other -> highlight color $ HH.text $ show other
        where
        color = typeToColor other

highlightTypeToHTML :: forall h a. Type -> HH.HTML h a
highlightTypeToHTML = highlightedType HH.span_ bold HT.highlight
