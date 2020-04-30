module Lunarbox.Page.Home
  ( Actions
  , home
  ) where

import Prelude
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (container)
import Lunarbox.Data.Route (Route(..))

type Actions a
  = { navigate :: Route -> Maybe a
    }

-- Static html for the home page
home :: forall a h. Actions a -> HH.HTML h a
home { navigate } =
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
                [ HP.id_ "free" ]
                [ HH.text "free" ]
            , HH.text "!"
            ]
        , container "action-buttons"
            [ HH.button [ HP.id_ "signup", onClick $ const $ navigate Register ] [ HH.text "Signup" ]
            , HH.button [ HP.id_ "signin", onClick $ const $ navigate Login ] [ HH.text "Login" ]
            ]
        ]
    ]
