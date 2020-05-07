module Lunarbox.Component.Loading
  ( loading
  ) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML (HTML)

type Actions a
  = {}

type Input
  = Unit

loading :: forall h a. Input -> Actions a -> HTML h a
loading state actions = HH.text "unimplemented"
