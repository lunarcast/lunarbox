module Lunarbox.Page.Editor.EmptyEditor
  ( illustrationEditor
  , emptyEditor
  , erroredEditor
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (container)

error :: String
error = "https://cdn.discordapp.com/attachments/672889285438865453/700703624371765298/undraw_server_down_s4lk.svg"

rocket :: String
rocket = "https://cdn.discordapp.com/attachments/427754177318486016/688733413313871894/undraw_Outer_space_drqu_3.svg"

illustrationEditor :: forall a b. String -> String -> HH.HTML a b
illustrationEditor src text =
  container "empty"
    [ container "illustration" [ HH.img [ HP.src src ] ]
    , container "text" [ HH.text text ]
    ]

emptyEditor :: forall a b. HH.HTML a b
emptyEditor = illustrationEditor rocket "You are not editing anything at the moment."

erroredEditor :: forall a b. String -> HH.HTML a b
erroredEditor = illustrationEditor error
