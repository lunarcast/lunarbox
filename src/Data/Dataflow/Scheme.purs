module Lunarbox.Data.Dataflow.Scheme (Scheme(..)) where

import Prelude
import Data.Foldable (fold)
import Lunarbox.Data.Dataflow.Type (TVar(..), Type)

data Scheme
  = Forall (Array TVar) Type

instance showScheme :: Show Scheme where
  show (Forall [] t) = show t
  show (Forall quantifiers t) = "forall" <> fold (quantifiers <#> (\(TV n) -> " " <> n)) <> ". " <> show t
