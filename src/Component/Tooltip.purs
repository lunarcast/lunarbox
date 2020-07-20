module Lunarbox.Component.Tooltip where

import Prelude
import Halogen.HTML (AttrName(..), IProp, attr)
import Halogen.HTML.Properties.ARIA as HA

data TooltipPosition
  = Top
  | TopLeft
  | TopRight
  | Bottom
  | BottomLeft
  | BottomRight
  | Left
  | Right

instance showTooltipPosition :: Show TooltipPosition where
  show Top = "top"
  show TopLeft = "top-left"
  show TopRight = "top-right"
  show Bottom = "bottom"
  show BottomLeft = "bottom-left"
  show BottomRight = "bottom-right"
  show Left = "left"
  show Right = "right"

-- | Wrapper around microtip.css
tooltip ::
  forall r i h.
  String ->
  TooltipPosition ->
  (Array (IProp r i) -> h) -> Array (IProp r i) -> h
tooltip text position element extraAttribs = element attribs
  where
  attribs =
    [ HA.role "tooltip"
    , HA.label text
    , attr (AttrName "data-microtip-position") $ show position
    ]
      <> extraAttribs
