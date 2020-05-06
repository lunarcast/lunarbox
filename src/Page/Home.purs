module Lunarbox.Page.Home
  ( Actions
  , home
  ) where

import Prelude
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Data.Route (Route(..))

type Actions a
  = { navigate :: Route -> Maybe a
    , logout :: Maybe a
    }

type Input
  = { guest :: Boolean
    }

-- Static html for the home page
home :: forall a h. Input -> Actions a -> HH.HTML h a
home { guest } { navigate, logout } =
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
            if guest then
              [ HH.text "Join lunarbox for "
              , HH.span
                  [ HP.id_ "free" ]
                  [ HH.text "free" ]
              , HH.text "!"
              ]
            else
              [ HH.text "Start creating!" ]
        , HH.div [ HP.id_ "action-buttons", className if guest then "guest" else "user" ]
            if guest then
              [ HH.button [ HP.id_ "primary", onClick $ const $ navigate Register ] [ HH.text "Signup" ]
              , HH.button [ HP.id_ "secondary", onClick $ const $ navigate Login ] [ HH.text "Login" ]
              ]
            else
              [ HH.button [ HP.id_ "primary", onClick $ const $ navigate Projects ] [ HH.text "Projects" ]
              , HH.button [ HP.id_ "logout", onClick $ const logout ] [ HH.text "or logout" ]
              ]
        ]
    ]
