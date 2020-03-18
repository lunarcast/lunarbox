module Lunarbox.Component.Icon where

import Prelude
import Halogen.HTML as HH
import Lunarbox.Component.Utils (StaticHtml, className)

icon :: forall a b. StaticHtml String a b
icon name = HH.i [ className "material-icons" ] [ HH.text name ]

iconInsideContainer :: forall a b. String -> StaticHtml String a b
iconInsideContainer class' = HH.div [ className class' ] <<< pure <<< icon
