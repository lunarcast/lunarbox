module Lunarbox.Page.Home where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as Hp
import Lunarbox.Component.Utils (StaticHtml, container)

-- Static html for the home page
home :: forall a b. StaticHtml Unit a b
home _ =
  container "home"
    [ container "bg" []
    , container "title-text"
        [ container "title"
            [ HH.text "Lunarbox"
            ]
        , container "description"
            [ HH.text "Functional programming made easy"
            ]
        ]
    , container "cta"
        [ container "cta-text"
            [ HH.text "Join lunarbox for "
            , HH.span
                [ Hp.id_ "free" ]
                [ HH.text "free" ]
            , HH.text "!"
            ]
        , container "action-buttons"
            [ HH.button [ Hp.id_ "signup" ] [ HH.text "Signup" ]
            , HH.button [ Hp.id_ "signin" ] [ HH.text "Login" ]
            ]
        ]
    ]
