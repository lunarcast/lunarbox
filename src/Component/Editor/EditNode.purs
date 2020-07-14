module Lunarbox.Data.Editor.EditNode
  ( component
  , Input
  , ChildSlots
  ) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Halogen (Slot, ComponentHTML)
import Halogen.HTML as HH
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Editor.NodeUi (hasUi)
import Lunarbox.Component.Editor.NodeUiManager as NodeUiManager
import Lunarbox.Component.Utils (className, maybeElement, whenElem)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type (Type)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)

type ChildSlots r
  = ( nodeUi :: Slot NodeUiManager.Query NodeUiManager.Output NodeId
    | r
    )

type Input a
  = { description :: Maybe String
    , type' :: Maybe Type
    , inputs ::
      Array
        { name :: String
        , type' :: Type
        , description :: String
        }
    , function :: FunctionName
    , id :: NodeId
    , value :: RuntimeValue
    , setValue :: RuntimeValue -> Maybe a
    }

-- | The content of the node editing modal
component :: forall a m r. Input a -> ComponentHTML a (ChildSlots r) m
component { description, type', inputs, id, function, value, setValue } =
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
          , HH.div [ className "edit-node__inputs-list" ]
              $ mkInput
              <$> inputs
          ]
    , whenElem (hasUi function) \_ ->
        HH.section [ className "edit-node__value" ]
          [ HH.div [ className "edit-node__value-lavel" ] [ HH.text "Value:" ]
          , HH.slot (SProxy :: SProxy "nodeUi") id
              NodeUiManager.component
              { name: function, value }
              handleNewValues
          ]
    ]
  where
  handleNewValues = case _ of
    NodeUiManager.NewValue val -> setValue val

  mkInput input =
    HH.details [ className "edit-node__input" ]
      [ HH.summary [ className "edit-node__input-name" ]
          [ HH.text input.name, HH.text " :: ", highlightTypeToHTML input.type'
          ]
      , HH.div [ className "edit-node__input-description" ] [ HH.text input.description ]
      ]
