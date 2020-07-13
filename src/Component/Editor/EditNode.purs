module Lunarbox.Data.Editor.EditNode
  ( component, Input
  ) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Utils (className, maybeElement, whenElem)
import Lunarbox.Data.Dataflow.Type (Type)

type Input
  = { description :: Maybe String
    , type' :: Maybe Type
    , inputs ::
      Array
        { name :: String
        , type' :: Type
        , description :: String
        }
    }

-- | The content of the node editing modal
component :: forall h a. Input -> HH.HTML h a
component { description, type', inputs } =
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
    , whenElem
        (not (Array.null inputs)) \_ ->
        HH.section [ className "edit-node__inputs" ]
          [ HH.h3 [ className "edit-node__inputs-header" ] [ HH.text "Inputs:" ]
          , HH.ul [ className "edit-node__inputs-list" ]
              $ mkInput
              <$> inputs
          ]
    ]
  where
  mkInput input =
    HH.li [ className "edit-node__input" ]
      [ HH.div [ className "edit-node__input-name" ]
          [ HH.text input.name, HH.text " :: ", highlightTypeToHTML input.type'
          ]
      ]
