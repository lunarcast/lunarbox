module Lunarbox.Component.WithLogo
  ( withLogo
  ) where

import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Constants (logoWithText)

-- Page with a logo on the left
withLogo :: forall h a. HTML h a -> HTML h a
withLogo content =
  HH.div [ className "with-logo" ]
    [ HH.div [ className "left" ]
        [ container "logo"
            [ HH.img
                [ HP.src logoWithText
                ]
            ]
        ]
    , HH.div [ className "right" ]
        [ content
        ]
    ]
