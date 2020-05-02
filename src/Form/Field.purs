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
  HH.div [ className "submit-container" ]
    [ HH.button
        [ className "submit-form"
        , onClick $ const $ Just F.submit
        -- , onClick $ const Nothing
        ]
        [ HH.text buttonText ]
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
input fieldSymbol form props =
  HH.div
    [ className "form-group" ]
    [ HH.input
        ( append
            [ className "form-field"
            , HP.value $ F.getInput fieldSymbol form
            , onValueInput $ Just <<< F.setValidate fieldSymbol
            ]
            props
        )
    , case (F.getError fieldSymbol form) of
        Just err ->
          HH.div
            [ className "error-message form-message" ]
            [ HH.text $ show err ]
        Nothing -> HH.div [ className "no-error form-message" ] [ HH.text "✔ Everything good" ]
    ]
