module Lunarbox.Data.Route where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Lunarbox.Data.ProjectId (ProjectId(..))
import Routing.Duplex (RouteDuplex', as, parse, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

data Route
  = Home
  | Playground
  | Login
  | Register
  | Settings
  | Projects
  | Project ProjectId

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

-- | Our codec will cause a compile-time error if we fail to handle any of our route cases.
routingCodec :: RouteDuplex' Route
routingCodec =
  root
    $ sum
        { "Home": noArgs
        , "Settings": "settings" / noArgs
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Playground": "playground" / noArgs
        , "Projects": "projects" / noArgs
        , "Project": "project" / projectId segment
        }

--  This combinator transforms a codec over `String` into one that operatos on the `ProjectId` type.
projectId :: RouteDuplex' String -> RouteDuplex' ProjectId
projectId = as show (Right <<< ProjectId)

parseRoute :: String -> Either RouteError Route
parseRoute = parse routingCodec
