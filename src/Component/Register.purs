module Lunarbox.Component.Register
  ( component
  , Input
  , ChildSlots
  , RegisterForm(..)
  , FormQuery(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Api.Requests (RegisterFields)
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Control.Monad.Effect (print)
import Lunarbox.Data.Profile (Email, Username)
import Lunarbox.Form.Field as Field
import Lunarbox.Form.Validation (emailValidators, passwordValidators, usernameValidators)
import Lunarbox.Form.Validation as V
import Lunarbox.Page.FormPage (formPage)

data Action
  = HandleRegisterForm RegisterFields

type Input
  = { redirect :: Boolean }

type ChildSlots
  = ( formless :: F.Slot RegisterForm FormQuery () RegisterFields Unit )

component :: forall q o m. MonadEffect m => MonadAff m => Component HH.HTML q Input o m
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
    HandleRegisterForm { email, password } -> do
      print email
      print password

  render _ =
    formPage "Register"
      $ HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)

-- Formless form for logging in
newtype RegisterForm r f
  = RegisterForm
  ( r
      ( email :: f V.FormError String Email
      , username :: f V.FormError String Username
      , password :: f V.FormError String String
      )
  )

derive instance newtypeRegisterForm :: Newtype (RegisterForm r f) _

data FormQuery a
  = SetRegisterError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

formComponent ::
  forall i slots m.
  MonadAff m =>
  F.Component RegisterForm FormQuery slots i RegisterFields m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleEvent = handleEvent
        , handleQuery = handleQuery
        }
  where
  formInput :: i -> F.Input RegisterForm ( loginError :: Boolean ) m
  formInput _ =
    { validators:
      RegisterForm
        { email: emailValidators
        , username: usernameValidators
        , password: passwordValidators
        }
    , initialInputs: Nothing
    , loginError: false
    }

  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetRegisterError bool a -> do
      modify_ _ { loginError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

  render { form, loginError } =
    HH.form_
      [ whenElem loginError \_ ->
          HH.div
            [ className "error-messages" ]
            [ HH.text "Email or password is invalid" ]
      , HH.fieldset_
          [ Field.input proxies.username form
              [ HP.placeholder "Username"
              , HP.type_ HP.InputText
              ]
          , Field.input proxies.email form
              [ HP.placeholder "Email"
              , HP.type_ HP.InputEmail
              ]
          , Field.input proxies.password form
              [ HP.placeholder "Password"
              , HP.type_ HP.InputPassword
              ]
          , Field.submit "Register"
          ]
      ]
