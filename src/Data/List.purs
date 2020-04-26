module Lunarbox.Data.List
  ( chunk
  , alterLast
  ) where

import Prelude
import Data.List (List(..), alterAt, drop, take, length, (:))
import Data.Maybe (Maybe(..), fromMaybe)

-- Split a List into chunks of a given size
chunk :: forall a. Int -> List a -> List (List a)
chunk _ Nil = Nil

chunk size list = take size list : chunk size (drop size list)

-- Helper to apply a function on the last element of a List
-- Accepts a default param in case the list is empty
alterLast :: forall a. a -> (a -> a) -> List a -> List a
alterLast defaultValue mapper = case _ of
  Nil -> pure $ mapper defaultValue
  list -> fromMaybe list $ alterAt (length list - 1) (Just <<< mapper) list
