module Lunarbox.Data.Editor.EditNode
  ( component, Input
  ) where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Lunarbox.Component.Utils (className, maybeElement)

type Input
  = { description :: Maybe String }

-- | The content of the node editing modal
component :: forall h a. Input -> HH.HTML h a
component { description } =
  HH.div [ className "edit-node" ]
    [ maybeElement description \text ->
        HH.section [ className "edit-node__description" ]
          [ HH.text text
          ]
    ]
