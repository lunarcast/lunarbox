module Lunarbox.Component.Register
  ( component
  , ChildSlots
  , RegisterForm(..)
  , FormQuery(..)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Formless as F
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval, modify_, query)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Api.Request (RegisterFields)
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.User (class ManageUser, registerUser)
import Lunarbox.Component.Utils (className, maybeElement)
import Lunarbox.Data.Profile (Email, Username)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Form.Field as Field
import Lunarbox.Form.Validation (emailValidators, passwordValidators, usernameValidators)
import Lunarbox.Form.Validation as V
import Lunarbox.Page.FormPage (formPage)

data Action
  = HandleRegisterForm RegisterFields
  | ToLogin

type ChildSlots
  = ( formless :: F.Slot RegisterForm FormQuery () RegisterFields Unit )

component :: forall q o m i. MonadEffect m => MonadAff m => Navigate m => ManageUser m => Component HH.HTML q i o m
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
  handleAction :: Action -> HalogenM i Action ChildSlots o m Unit
  handleAction = case _ of
    HandleRegisterForm fields -> do
      registerUser fields
        >>= case _ of
            Left err -> void $ query F._formless unit $ F.injQuery $ SetRegisterError (Just err) unit
            Right profile -> do
              void $ query F._formless unit $ F.injQuery $ SetRegisterError Nothing unit
              navigate Home
    ToLogin -> navigate Login

  render _ =
    formPage
      { title: "Register"
      , content: HH.slot F._formless unit formComponent unit (Just <<< HandleRegisterForm)
      , message: "Already have an account? "
      , action: "Log in!"
      , onAction: Just ToLogin
      }

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
  = SetRegisterError (Maybe String) a

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
  formInput :: i -> F.Input RegisterForm ( registerError :: Maybe String ) m
  formInput _ =
    { validators:
      RegisterForm
        { email: emailValidators
        , username: usernameValidators
        , password: passwordValidators
        }
    , initialInputs: Nothing
    , registerError: Nothing
    }

  handleEvent = F.raiseResult

  handleQuery :: forall a. FormQuery a -> HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    SetRegisterError bool a -> do
      modify_ _ { registerError = bool }
      pure (Just a)

  proxies = F.mkSProxies (F.FormProxy :: _ RegisterForm)

  render { form, registerError } =
    HH.div [ className "form" ]
      [ maybeElement registerError \err ->
          HH.div
            [ className "form__message form__message--error " ]
            [ HH.text err ]
      , HH.div_
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
