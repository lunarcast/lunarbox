module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen (Component, hoist, tell)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML (HTML)
import Halogen.VDom.Driver (runUI)
import Lunarbox.AppM (runAppM)
import Lunarbox.Component.Router as Router
import Lunarbox.Config (Config(..))
import Lunarbox.Data.Route (routingCodec)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main =
  runHalogenAff do
    nav <- liftEffect makeInterface
    body <- awaitBody
    -- TODO: make this depend on some .env file
    let
      env :: Config
      env = Config { devOptions: Just { cancelInputsOnBlur: true } }

      rootComponent :: Component HTML Router.Query {} Void Aff
      rootComponent = hoist (runAppM env) Router.component
    halogenIO <- runUI rootComponent {} body
    let
      onRouteChange = \old new ->
        when (old /= Just new) do
          launchAff_ $ halogenIO.query $ tell $ Router.Navigate $ new
    void $ liftEffect
      $ matchesWith (parse routingCodec) onRouteChange nav
    pure unit
