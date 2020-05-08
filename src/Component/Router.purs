module Lunarbox.Component.Router (component, Query(..)) where

import Prelude
import Control.Monad.Reader (class MonadReader, asks)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.Lens (view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, get, liftEffect, mkComponent, mkEval, modify_)
import Halogen.HTML as HH
import Lunarbox.Capability.Navigate (class Navigate, logout, navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects)
import Lunarbox.Capability.Resource.User (class ManageUser)
import Lunarbox.Component.HOC.Connect (WithCurrentUser)
import Lunarbox.Component.HOC.Connect as Connect
import Lunarbox.Component.Login as Login
import Lunarbox.Component.Project as ProjectC
import Lunarbox.Component.Projects as ProjectsC
import Lunarbox.Component.Register as Register
import Lunarbox.Component.Utils (OpaqueSlot)
import Lunarbox.Config (Config, _locationState)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Profile (Profile)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.Route (Route(..), parseRoute)
import Lunarbox.Page.Home (home)
import Record as Record

type State
  = { route :: Maybe Route
    , currentUser :: Maybe Profile
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Logout
  | NavigateTo Route
  | Receive { | WithCurrentUser () }

type ChildSlots
  = ( settings :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , register :: OpaqueSlot Unit
    , "projects" :: OpaqueSlot Unit
    , "project" :: OpaqueSlot ProjectId
    )

type ComponentM
  = HalogenM State Action ChildSlots Void

-- Array of pages which cannot be accessed by logged in users
noAuth :: Array Route
noAuth = [ Login, Register ]

component ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  Navigate m =>
  MonadReader Config m =>
  ManageProjects m =>
  ManageUser m => Component HH.HTML Query {} Void m
component =
  Connect.component
    $ mkComponent
        { initialState:
          const
            { route: Nothing
            , currentUser: Nothing
            }
        , render
        , eval:
          mkEval
            $ defaultEval
                { handleQuery = handleQuery
                , handleAction = handleAction
                , initialize = Just Initialize
                , receive = Just <<< Receive
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
    Receive newData -> do
      modify_ $ Record.merge newData
    NavigateTo destination -> do
      { route, currentUser } <- get
      if isJust currentUser && (isJust $ find (_ == destination) noAuth) then do
        navigate $ fromMaybe Home route
      else do
        when (route /= Just destination) $ navigate destination
    Logout -> logout

  -- Handle queries from the outside world
  -- The navigate query is called every time the url changes
  handleQuery :: forall a. Query a -> ComponentM m (Maybe a)
  handleQuery = case _ of
    Navigate destination a -> do
      { route } <- get
      when (route /= Just destination) $ modify_ (_ { route = Just destination })
      pure $ Just a

  -- Display the login page instead of the expected page if there is no current user; a simple
  -- way to restrict access.
  authorize :: Maybe Profile -> HH.ComponentHTML Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  authorize mbProfile html = case mbProfile of
    Nothing -> HH.slot (SProxy :: _ "login") unit Login.component { redirect: false } absurd
    Just _ -> html

  notFound =
    HH.div_
      [ HH.text "404 - Page not found" ]

  render { route, currentUser } =
    route
      <#> case _ of
          Home -> home { guest: isNothing currentUser } { navigate: Just <<< NavigateTo, logout: Just Logout }
          Login -> HH.slot (SProxy :: _ "login") unit Login.component { redirect: true } absurd
          Register -> HH.slot (SProxy :: _ "register") unit Register.component unit absurd
          Projects -> requireAuthorization $ HH.slot (SProxy :: _ "projects") unit ProjectsC.component {} absurd
          Project id -> requireAuthorization $ HH.slot (SProxy :: _ "project") id ProjectC.component { id } absurd
      # fromMaybe notFound
    where
    requireAuthorization = authorize currentUser
