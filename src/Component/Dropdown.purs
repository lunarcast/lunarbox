module Lunarbox.Component.Dropdown
  ( Slot
  , Query(..)
  , Input
  , Message
  , _dropdown
  , clear
  , spec
  -- Those are reused by the typeahead
  , toggle
  , menu
  ) where

import Prelude
import DOM.HTML.Indexed (HTMLbutton)
import Data.Array (difference, mapWithIndex, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Lunarbox.Component.Utils (className)
import Select as Select
import Select.Setters as Setters

type Slot item
  = H.Slot (Select.Query Query ()) (Message item)

_dropdown = SProxy :: SProxy "dropdown"

data Query a
  = Clear a

clear :: Select.Query Query () Unit
clear = Select.Query (H.tell Clear)

type State item
  = ( selected :: Maybe item
    , available :: Array item
    , items :: Array item
    , placeholder :: String
    )

type Input item
  = { items :: Array item
    , placeholder :: String
    }

input :: forall item. Input item -> Select.Input (State item)
input { items, placeholder } =
  { inputType: Select.Toggle
  , search: Nothing
  , debounceTime: Nothing
  , getItemCount: length <<< _.items
  , selected: Nothing
  , available: items
  , items
  , placeholder
  }

data Message item
  = Selected item
  | Cleared

spec ::
  forall item m i.
  MonadAff m =>
  Show item =>
  Eq item =>
  Select.Spec (State item) Query Void () i (Message item) m
spec =
  Select.defaultSpec
    { render = render
    , handleQuery = handleQuery
    , handleEvent = handleEvent
    }
  where
  render st =
    HH.div
      [ className
          if st.visibility == Select.On then
            "dropdown dropdow--is-active"
          else
            "dropdown"
      ]
      [ toggle [] st, menu st ]

  handleQuery :: forall a. Query a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = case _ of
    Clear a -> do
      H.modify_ \st -> st { selected = Nothing, available = st.items }
      H.raise Cleared
      pure (Just a)

  handleEvent = case _ of
    Select.Selected ix -> do
      st <- H.get
      let
        mbItem = st.available !! ix
      for_ mbItem \item -> do
        H.modify_
          _
            { selected = Just item
            , available = difference st.items [ item ]
            , visibility = Select.Off
            }
        H.raise (Selected item)
    _ -> pure unit

toggle ::
  forall item act ps m r.
  Show item =>
  Array (HH.IProp HTMLbutton (Select.Action act)) ->
  { placeholder :: String, selected :: Maybe item | r } ->
  H.ComponentHTML (Select.Action act) ps m
toggle props st =
  HH.div
    [ className "dropdown__trigger" ]
    [ HH.button
        (Setters.setToggleProps $ props <> [ className "dropdown__trigger-button" ])
        [ HH.text $ fromMaybe st.placeholder (show <$> st.selected) ]
    ]

menu ::
  forall item st act ps m.
  Show item =>
  Select.State ( available :: Array item | st ) ->
  H.ComponentHTML (Select.Action act) ps m
menu st =
  HH.div
    [ className "dropdown__menu" ]
    [ if st.visibility == Select.Off then
        HH.text ""
      else
        HH.div
          (Setters.setContainerProps [ className "dropdown__content" ])
          ( mapWithIndex
              ( \ix item ->
                  HH.div
                    ( Setters.setItemProps ix case Just ix == st.highlightedIndex of
                        true -> [ className "dropdown__item" ]
                        _ -> [ className "dropdown__item" ]
                    )
                    [ HH.text (show item) ]
              )
              st.available
          )
    ]
