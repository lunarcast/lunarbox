module Lunarbox.Component.Router where

import Prelude
import Control.Monad.Reader (class MonadReader, asks)
import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, Slot, defaultEval, get, liftEffect, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.User (class ManageUser)
import Lunarbox.Component.Editor as Editor
import Lunarbox.Component.Login as Login
import Lunarbox.Component.Register as Register
import Lunarbox.Component.Utils (OpaqueSlot)
import Lunarbox.Config (Config, _locationState)
import Lunarbox.Control.Monad.Effect (print, printString)
import Lunarbox.Data.Route (Route(..), parseRoute)
import Lunarbox.Page.Home (home)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | NavigateTo Route

type ChildSlots
  = ( editor :: Slot Editor.Query Void Unit
    , settings :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , register :: OpaqueSlot Unit
    )

type ComponentM
  = HalogenM State Action ChildSlots Void

component ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  Navigate m =>
  MonadReader Config m =>
  ManageUser m => Component HH.HTML Query {} Void m
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
      getLocationState <- asks $ view _locationState
      { path } <- liftEffect getLocationState
      case parseRoute path of
        Left error -> do
          printString $ "An error occured trying to parse the curent route. Defaulting to Home. " <> show error
          navigate Home
        Right route -> navigate route
    NavigateTo destination -> do
      print "called navigateTo"
      { route } <- get
      when (route /= Just destination) $ navigate destination

  handleQuery :: forall a. Query a -> ComponentM m (Maybe a)
  handleQuery = case _ of
    Navigate destination a -> do
      { route } <- get
      when (route /= Just destination) $ modify_ (_ { route = Just destination })
      pure $ Just a

  notFound =
    HH.div_
      [ HH.text "404 - Page not found" ]

  render { route } =
    route
      <#> case _ of
          Home -> home { navigate: Just <<< NavigateTo }
          Playground -> HH.slot (SProxy :: _ "editor") unit Editor.component {} absurd
          Login -> HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
          Register -> HH.slot (SProxy :: _ "register") unit Register.component unit absurd
          _ -> HH.text "not implemented"
      # fromMaybe notFound
