module Lunarbox.Component.Router where

import Prelude
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen (Component, liftEffect, HalogenM, defaultEval, get, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Component.Utils (OpaqueSlot)
import Lunarbox.Data.Route (Route(..), parseRoute)
import Routing.Hash (getHash)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots
  = ( home :: OpaqueSlot Unit
    , playground :: OpaqueSlot Unit
    , settings :: OpaqueSlot Unit
    )

type ComponentM m a
  = HalogenM State Action ChildSlots Void m a

component :: forall m. MonadEffect m => Navigate m => Component HH.HTML Query {} Void m
component =
  mkComponent
    { initialState: const { route: Nothing }
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              , initialize = Just Initialize
              }
    }
  where
  handleAction :: Action -> ComponentM m Unit
  handleAction = case _ of
    Initialize -> do
      liftEffect $ logShow $ (hush <<< parseRoute) "/"
      h <- liftEffect getHash
      liftEffect $ logShow h
      initialRoute <- hush <<< parseRoute <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute

  handleQuery :: forall a. Query a -> ComponentM m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- get
      when (route /= Just dest) $ modify_ (_ { route = Just dest })
      pure (Just a)

  notFound =
    HH.div_
      [ HH.text "404 - Page not found" ]

  render { route } =
    route
      <#> case _ of
          Home -> HH.text "home"
          Settings -> HH.text "settings"
          Playground -> HH.text "playground"
      # fromMaybe notFound
