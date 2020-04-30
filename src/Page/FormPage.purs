module Lunarbox.Page.FormPage (formPage) where

import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (container)
import Lunarbox.Constants (logoWithText)

formPage :: forall h a. String -> HTML h a -> HTML h a
formPage title inner =
  container "form-page"
    [ container "form-left"
        [ container "logo"
            [ HH.img
                [ HP.src logoWithText
                ]
            ]
        ]
    , container "form-right"
        [ container "form-container"
            [ container "title"
                [ HH.text title
                ]
            , inner
            ]
        ]
    ]
