module Lunarbox.Form.Field where

import Prelude
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy, Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)
import Lunarbox.Form.Validation as V
import Type.Row as Row

-- Reusable submit button
submit :: forall form act slots m. String -> F.ComponentHTML form act slots m
submit buttonText =
  HH.div [ className "submit__container" ]
    [ HH.button
        [ className "submit__form"
        , onClick $ const $ Just F.submit
        -- , onClick $ const Nothing
        ]
        [ HH.text buttonText ]
    ]

-- | Wrapper for any kind of form field
customFormField ::
  forall form act slots m sym fields inputs from to t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError from to) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError from to) t1 inputs =>
  SProxy sym ->
  form Record F.FormField ->
  F.ComponentHTML form act slots m ->
  F.ComponentHTML form act slots m
customFormField proxy form content =
  HH.div [ className "form__group" ]
    [ content
    , case (F.getError proxy form) of
        Just err ->
          HH.div
            [ className "form__message--error form__message" ]
            [ HH.text $ show err ]
        Nothing -> HH.div [ className "form__message--no-error form__message" ] [ HH.text "✔ Everything good" ]
    ]

-- Modified version of  https://github.com/thomashoneyman/purescript-halogen-realworld/blob/master/src/Form/Field.purs
input ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  SProxy sym ->
  form Record F.FormField ->
  Array (HH.IProp HTMLinput (F.Action form act)) ->
  F.ComponentHTML form act slots m
input fieldSymbol form props = customFormField fieldSymbol form html
  where
  html =
    HH.input
      $ append
          [ className "form__field form__field--text"
          , HP.value $ F.getInput fieldSymbol form
          , onValueInput $ Just <<< F.setValidate fieldSymbol
          ]
          props
