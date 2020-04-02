module Lunarbox.Data.Dataflow.Scheme (Scheme(..)) where

import Prelude
import Data.Foldable (fold)
import Lunarbox.Data.Dataflow.Type (TVarName(..), Type)

data Scheme
  = Forall (Array TVarName) Type

instance showScheme :: Show Scheme where
  show (Forall [] t) = show t
  show (Forall quantifiers t) = "forall" <> fold (quantifiers <#> (\(TVarName n) -> " " <> n)) <> ". " <> show t
