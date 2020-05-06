module Lunarbox.Page.FormPage (formPage) where

import Prelude
import Data.Maybe (Maybe)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (container)
import Lunarbox.Component.WithLogo (withLogo)

type Input h a
  = { title :: String
    , message :: String
    , action :: String
    , onAction :: Maybe a
    , content :: HTML h a
    }

formPage :: forall h a. Input h a -> HTML h a
formPage { title, content, action, message, onAction } =
  container "form-page"
    [ withLogo
        $ container "form-container"
            [ container "title-row"
                [ container "title" [ HH.text title ]
                , container "message"
                    [ container "message-normal" [ HH.text message ]
                    , HH.div
                        [ HP.id_ "message-action"
                        , onClick $ const onAction
                        ]
                        [ HH.text action ]
                    ]
                ]
            , content
            ]
    ]
