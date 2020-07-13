module Lunarbox.Data.Editor.EditNode
  ( component, Input
  ) where

import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Utils (className, maybeElement)
import Lunarbox.Data.Dataflow.Type (Type)

type Input
  = { description :: Maybe String
    , type' :: Maybe Type
    }

-- | The content of the node editing modal
component :: forall h a. Input -> HH.HTML h a
component { description, type' } =
  HH.div [ className "edit-node" ]
    [ maybeElement type' \type'' ->
        HH.section [ className "edit-node__type" ]
          [ HH.text ":: "
          , highlightTypeToHTML type''
          ]
    , maybeElement description \text ->
        HH.section [ className "edit-node__description" ]
          [ HH.text text
          ]
    ]
