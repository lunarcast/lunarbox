module Lunarbox.Component.Login
  ( component
  , Input
  , ChildSlots
  , LoginForm(..)
  , FormQuery(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless (injQuery)
import Formless as F
import Halogen (Component, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, query)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Api.Request (LoginFields)
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.User (class ManageUser, loginUser)
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Data.Profile (Email)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Form.Field as Field
import Lunarbox.Form.Validation (emailValidators, passwordValidators)
import Lunarbox.Form.Validation as V
import Lunarbox.Page.FormPage (formPage)

data Action
  = HandleLoginForm LoginFields

type Input
  = { redirect :: Boolean }

type ChildSlots
  = ( formless :: F.Slot LoginForm FormQuery () LoginFields Unit )

component :: forall q o m. MonadEffect m => MonadAff m => ManageUser m => Navigate m => Component HH.HTML q Input o m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            }
    }
  where
  handleAction :: Action -> HalogenM Input Action ChildSlots o m Unit
  handleAction = case _ of
    HandleLoginForm fields -> do
      loginUser fields
        >>= case _ of
            Nothing -> void $ query F._formless unit $ injQuery $ SetLoginError true unit
            Just profile -> do
              void $ query F._formless unit $ injQuery $ SetLoginError false unit
              st <- get
              when st.redirect $ navigate Home

  render _ =
    formPage "Login"
      $ HH.slot F._formless unit formComponent unit (Just <<< HandleLoginForm)

-- Formless form for logging in
newtype LoginForm r f
  = LoginForm
  ( r
      ( email :: f V.FormError String Email
      , password :: f V.FormError String String
      )
  )

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component LoginForm FormQuery slots i LoginFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  formInput :: i -> F.Input LoginForm ( loginError :: Boolean ) m
  formInput _ =
    { validators:
      LoginForm
        { email: emailValidators
        , password: passwordValidators
        }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetLoginError bool a -> do
      modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

  render { form, loginError } =
    HH.form_
      [ whenElem loginError \_ ->
          HH.div
            [ className "error-messages" ]
            [ HH.text "Email or password is invalid" ]
      , HH.fieldset_
          [ Field.input proxies.email form
              [ HP.placeholder "Email"
              , HP.type_ HP.InputEmail
              ]
          , Field.input proxies.password form
              [ HP.placeholder "Password"
              , HP.type_ HP.InputPassword
              ]
          , Field.submit "Log in"
          ]
      ]
