module Lunarbox.Component.Modal
  ( Query(..)
  , Action(..)
  , Output(..)
  , Input
  , Slot
  , ButtonConfig
  , InputType
  , component
  ) where

import Prelude
import Control.Monad.State (get, gets, put)
import Control.MonadZero (guard)
import Control.Promise (Promise, toAff)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (AttrName(..), ClassName(..), Component, ComponentSlot, HalogenM, defaultEval, fork, mkComponent, mkEval, raise)
import Halogen.Data.Slot as HSlot
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as AP
import Lunarbox.Component.Utils (className)
import Web.HTML (HTMLElement)

foreign import showModal :: String -> Effect (Promise HTMLElement)

foreign import closeModal :: String -> Effect Unit

data Action a v
  = CloseModal v
  | Bubble a

-- | Query (parent action) (output type) monad result 
data Query cs pa v m a
  = Close a
  | Open a
  | UpdateInput (InputType cs v pa m) a

-- | Helper for including the modal as a children
type Slot pa cs v m
  = HSlot.Slot (Query cs pa v m) (Output pa v)

-- | Config for how a button should act & look
type ButtonConfig v
  = { text :: String
    -- TODO: make this more generic once I make a separate button component or something
    , primary :: Boolean
    , value :: v
    }

type Input h a v pa r
  = ( id :: String
    , title :: String
    , content :: (pa -> a) -> HH.HTML h a
    , buttons :: Array (ButtonConfig v)
    , onClose :: v
    | r
    )

type State h a v pa
  = Input h a v pa ()

_open :: forall r. Lens' { open :: Boolean | r } Boolean
_open = prop (SProxy :: SProxy "open")

type InputType cs v pa m
  = { 
    | Input (ComponentSlot HH.HTML cs m (Action pa v))
      (Action pa v)
      v
      pa
      ()
    }

data Output pa v
  = ClosedWith v
  | BubbledAction pa

component ::
  forall m v pa (cs :: #Type).
  MonadEffect m =>
  MonadAff m => Component HH.HTML (Query cs pa v m) (InputType cs v pa m) (Output pa v) m
component =
  mkComponent
    { initialState: identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  handleAction ::
    Action pa v ->
    HalogenM { | State _ _ v pa } (Action pa v) cs
      (Output pa v)
      m
      Unit
  handleAction = case _ of
    CloseModal value -> do
      id <- gets _.id
      liftEffect $ closeModal id
    Bubble a -> raise $ BubbledAction a

  handleQuery ::
    forall a.
    Query cs pa v m a ->
    HalogenM { | State _ _ v pa }
      (Action pa v)
      cs
      (Output pa v)
      m
      (Maybe a)
  handleQuery = case _ of
    Open return -> do
      { id, onClose } <- get
      void
        $ fork do
            promise <- liftEffect $ showModal id
            void $ liftAff $ toAff promise
            raise $ ClosedWith onClose
      pure $ Just return
    Close a -> do
      id <- gets _.id
      liftEffect $ closeModal id
      pure $ Just a
    UpdateInput input result -> do
      put input
      pure $ Just result

  render { id, title, content, buttons } =
    HH.div
      [ HP.id_ id
      , AP.hidden "true"
      , className "modal micromodal-slide"
      ]
      [ HH.div
          [ HP.tabIndex (-1)
          , HP.attr (AttrName "data-micromodal-close") "true"
          , className "modal__overlay"
          ]
          [ HH.div
              [ AP.role "dialog"
              , HP.attr (AttrName "aria-modal") "true"
              , AP.labelledBy titleId
              , className "modal__container"
              ]
              [ HH.header [ className "modal__header" ]
                  [ HH.h2 [ HP.id_ titleId, className "modal__title" ]
                      [ HH.text title
                      ]
                  ]
              , HH.main [ HP.id_ contentId, className "modal__content" ]
                  [ content Bubble
                  ]
              , HH.footer [ className "modal__footer" ]
                  $ ( \{ text, primary, value } ->
                        HH.button
                          [ HP.classes
                              $ ClassName
                              <$> ("modal__btn-primary" <$ guard primary)
                              <> [ "modal__btn" ]
                          , onClick $ const $ Just $ CloseModal value
                          ]
                          [ HH.text text ]
                    )
                  <$> buttons
              ]
          ]
      ]
    where
    titleId = id <> "-title"

    contentId = id <> "-content"
