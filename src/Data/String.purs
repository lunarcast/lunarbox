module Lunarbox.Data.String where

import Prelude
import Data.Either (Either, either)
import Data.Foldable (foldr)
import Data.String (joinWith)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines, unsafeRepeat)
import Halogen.HTML as HH

-- Indent a string by a number of spaces
indent :: Int -> String -> String
indent spaces = joinWith "\n" <<< map (space <> _) <<< lines
  where
  space = unsafeRepeat spaces " "

-- Replaces \n with <br>
toHtml :: forall h a. String -> HH.HTML h a
toHtml text =
  HH.span_
    $ foldr
        (\line -> flip append [ HH.br_, HH.text line ])
        []
    $ lines
        text

-- Put spaces around a string
spaced :: String -> String
spaced s = " " <> s <> " "

-- Helper to create a validator from either an error or a regex
validatorFromRegex :: forall a. Either a Regex -> String -> Boolean
validatorFromRegex = either (const $ const false) test

-- CHeck if a string contains at least an uppercase character
hasUppercase :: String -> Boolean
hasUppercase = validatorFromRegex $ regex "[A-Z]" noFlags

-- Check if a string cnotains at least a lowecase character
hasLowecase :: String -> Boolean
hasLowecase = validatorFromRegex $ regex "[a-z]" noFlags

-- Check if a string contains at least a digit
containsDigits :: String -> Boolean
containsDigits = validatorFromRegex $ regex ".*[0-9].*" noFlags
