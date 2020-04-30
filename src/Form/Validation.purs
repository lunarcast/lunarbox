module Lunarbox.Form.Validation where

import Prelude
import Data.Either (Either(..))
import Data.String as String
import Formless (Validation)
import Formless as F
import Lunarbox.Data.Profile (Email(..))
import Lunarbox.Data.String (containsDigits, hasLowecase, hasUppercase)

data FormError
  = Required
  | TooShort Int
  | TooLong Int
  | NoUppercase
  | NoLowecase
  | NoDigits
  | InvalidEmail
  | InvalidUsername

instance showFormError :: Show FormError where
  show error =
    "✖ "
      <> case error of
          Required -> "This field is required."
          TooShort min -> "This field needs at least " <> show min <> " characters."
          TooLong max -> "This fields cannot include more than " <> show max <> " characters."
          InvalidEmail -> "Invalid email address."
          InvalidUsername -> "Invalid username."
          NoUppercase -> "This field needs at least an uppercase character."
          NoLowecase -> "This field needs at least a lowecase character."
          NoDigits -> "This field needs at least a digit."

-- Helper to validate form errors based on a predicate
condition :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
condition predicate err value =
  if predicate value then
    pure value
  else
    Left err

-- Mark a field as required
required :: forall form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ condition (_ /= mempty) Required

-- The next 2 helpers handle strings which are either too short or too long
maxLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
maxLength length = F.hoistFnE_ $ condition ((_ <= length) <<< String.length) $ TooLong length

minLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
minLength length = F.hoistFnE_ $ condition ((_ >= length) <<< String.length) $ TooShort length

-- Validators for strings which require a specific types of characters
needsUppercase :: forall form m. Monad m => F.Validation form m FormError String String
needsUppercase = F.hoistFnE_ $ condition hasUppercase NoUppercase

needsLowecase :: forall form m. Monad m => F.Validation form m FormError String String
needsLowecase = F.hoistFnE_ $ condition hasLowecase NoLowecase

hasDigit :: forall form m. Monad m => F.Validation form m FormError String String
hasDigit = F.hoistFnE_ $ condition containsDigits NoDigits

-- Very basic email validator. Checks if the string contains @ 
email :: ∀ form m. Monad m => F.Validation form m FormError String Email
email = F.hoistFnE_ $ map Email <<< condition (String.contains $ String.Pattern "@") InvalidEmail

-- Validators for the password
passwordValidators :: forall m a. Monad m => Validation a m FormError String String
passwordValidators = required >>> minLength 8 >>> maxLength 32 >>> needsUppercase >>> needsLowecase >>> hasDigit

-- Validators for the email
emailValidators :: forall m a. Monad m => Validation a m FormError String Email
emailValidators = required >>> minLength 3 >>> email
