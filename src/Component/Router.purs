module Lunarbox.Component.Router where

import Prelude
import Control.Monad.Reader (class MonadReader)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, get, liftEffect, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Utils (OpaqueSlot)
import Lunarbox.Config (Config)
import Lunarbox.Data.Route (Route(..), parseRoute)
import Lunarbox.Page.Home (home)
import Routing.Hash (getHash)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize

type ChildSlots
  = ( editor :: Slot Editor.Query Void Unit
    , settings :: OpaqueSlot Unit
    )

type ComponentM m a
  = HalogenM State Action ChildSlots Void m a

component :: forall m. MonadEffect m => Navigate m => MonadReader Config m => Component HH.HTML Query {} Void m
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
          Home -> home unit
          Settings -> HH.text "settings"
          Playground -> HH.slot (SProxy :: _ "editor") unit Editor.component {} absurd
      # fromMaybe notFound
