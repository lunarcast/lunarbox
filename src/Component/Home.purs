module Lunarbox.Component.Home (component) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Navigate (class Navigate, logout, navigate)
import Lunarbox.Component.HOC.Connect as Connect
import Lunarbox.Component.Utils (className, container)
import Lunarbox.Config (Config)
import Lunarbox.Constants (transparentLogo)
import Lunarbox.Data.Profile (Profile)
import Lunarbox.Data.Route (Route(..))
import Record as Record

type State
  = { currentUser :: Maybe Profile
    }

data Action
  = Logout
  | NavigateTo Route
  | Receive { | Connect.WithCurrentUser () }

type ChildSlots
  = ()

component ::
  forall m q o.
  MonadEffect m =>
  MonadAff m =>
  MonadAsk Config m =>
  Navigate m => Component HH.HTML q {} o m
component =
  Connect.component
    $ mkComponent
        { initialState:
          const
            { currentUser: Nothing
            }
        , render
        , eval:
          mkEval
            $ defaultEval
                { handleAction = handleAction
                , receive = Just <<< Receive
                }
        }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    NavigateTo route -> navigate route
    Receive newData -> do
      modify_ $ Record.merge newData
    Logout -> logout

  render state =
    container "home"
      [ container "bg" []
      , container "header"
          [ HH.img [ HP.src transparentLogo, HP.id_ "logo" ]
          , container "title-text"
              [ container "title"
                  [ HH.text "Lunarbox"
                  ]
              , container "description"
                  [ HH.text "Functional programming made easy"
                  ]
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
                [ HH.button [ HP.id_ "primary", onClick $ const $ Just $ NavigateTo Register ] [ HH.text "Signup" ]
                , HH.button [ HP.id_ "secondary", onClick $ const $ Just $ NavigateTo Login ] [ HH.text "Login" ]
                ]
              else
                [ HH.button [ HP.id_ "primary", onClick $ const $ Just $ NavigateTo Projects ] [ HH.text "Projects" ]
                , HH.button [ HP.id_ "logout", onClick $ const $ Just Logout ] [ HH.text "or logout" ]
                ]
          ]
      ]
    where
    guest = isNothing state.currentUser
