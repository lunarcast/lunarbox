module Lunarbox.Api.Endpoint
  ( Endpoint(..)
  , endpointCodec
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, string, prefix)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- Possible endpoints we can hit
data Endpoint
  = Login
  | Logout
  | Profile
  | Register
  | Projects
  | Project String

derive instance eqEndpoint :: Eq Endpoint

derive instance ordEndpoint :: Ord Endpoint

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- This is here so we get compile time errors when we don't handle a route
endpointCodec :: RouteDuplex' Endpoint
endpointCodec =
  root $ prefix "api"
    $ sum
        { "Login": "auth" / "login" / noArgs
        , "Register": "users" / noArgs
        , "Logout": "auth" / "lougout" / noArgs
        , "Profile": "users" / noArgs
        , "Projects": "projects" / noArgs
        , "Project": "projects" / string segment
        }
