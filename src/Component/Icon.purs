module Lunarbox.Component.Icon where

import Halogen.HTML as HH
import Lunarbox.Component.Tooltip as Tooltip
import Lunarbox.Component.Utils (StaticHtml, className)

-- | Helper for using material icons
icon :: forall a b. StaticHtml String a b
icon name = HH.i [ className "material-icons" ] [ HH.text name ]

-- | Tooltip integration for material icons
iconWithTooltip :: forall a b. String -> Tooltip.TooltipPosition -> String -> HH.HTML a b
iconWithTooltip tooltipText position name =
  Tooltip.tooltip
    tooltipText
    position
    HH.i
    [ className "material-icons" ]
    [ HH.text name ]
