module Lunarbox.Form.Validation where

import Prelude
import Data.Either (Either(..))
import Data.String as String
import Formless as F
import Lunarbox.Data.Profile (Email(..))

data FormError
  = Required
  | TooShort Int
  | TooLong Int
  | InvalidEmail
  | InvalidUsername

instance showFormError :: Show FormError where
  show error =
    "✖ "
      <> case error of
          Required -> "This field is required."
          TooShort min -> "This field needs at least " <> show min <> " characters."
          TooLong max -> "This fields cannot include more than " <> show max <> " characters."
          InvalidEmail -> "Invalid email address"
          InvalidUsername -> "Invalid username"

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

-- Very basic email validator. Checks if the string contains @ 
email :: ∀ form m. Monad m => F.Validation form m FormError String Email
email = F.hoistFnE_ $ map Email <<< condition (String.contains $ String.Pattern "@") InvalidEmail
