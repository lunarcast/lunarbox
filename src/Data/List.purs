module Lunarbox.Data.List (chunk) where

import Data.List (take, drop, List(..), (:))

-- Split a List into chunks of a given size
chunk :: forall a. Int -> List a -> List (List a)
chunk _ Nil = Nil

chunk size list = take size list : chunk size (drop size list)
