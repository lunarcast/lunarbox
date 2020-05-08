module Lunarbox.Component.Error
  ( error
  ) where

import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)

-- Illustration for errors
dreamer :: String
dreamer = "https://cdn.discordapp.com/attachments/672889285438865453/708301925510283264/undraw_dreamer_gxxi.png"

-- A component which shows up when something errored out
error :: forall h a. String -> HTML h a
error text =
  HH.div [ className "error-container" ]
    [ HH.img
        [ HP.src dreamer
        , HP.alt "error"
        , className "error-illustration"
        ]
    , HH.div [ className "error-text" ] [ HH.text text ]
    ]
