module Lunarbox.Page.Editor.EmptyEditor where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (StaticHtml, container)

rocket :: String
rocket = "https://cdn.discordapp.com/attachments/427754177318486016/688733413313871894/undraw_Outer_space_drqu_3.svg"

emptyEditor :: forall a b. StaticHtml Unit a b
emptyEditor _ =
  container "empty"
    [ container "illustration" [ HH.img [ HP.src rocket ] ]
    , container "text" [ HH.text "You are not editing anything at the moment." ]
    ]
