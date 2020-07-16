module Lunarbox.Data.Route where

import Prelude
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Lunarbox.Data.ProjectId (ProjectId(..))
import Lunarbox.Data.Tutorial (TutorialId(..))
import Routing.Duplex (RouteDuplex', as, parse, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)

data Route
  = Home
  | Login
  | Register
  | Projects
  | Project ProjectId
  | EditTutorial TutorialId
  | Tutorial TutorialId
  | Clone ProjectId

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
        , "Login": "login" / noArgs
        , "Register": "register" / noArgs
        , "Projects": "projects" / noArgs
        , "EditTutorial": "edit" / "tutorial" / tutorialId segment
        , "Project": "edit" / "project" / projectId segment
        , "Tutorial": "go" / "tutorial" / tutorialId segment
        , "Clone": "go" / "project" / projectId segment
        }

--  This combinator transforms a codec over `String` into one that operatos on the `ProjectId` type.
projectId :: RouteDuplex' String -> RouteDuplex' ProjectId
projectId = as show (map ProjectId <<< note "Cannot parse project id" <<< fromString)

--  This combinator transforms a codec over `String` into one that operatos on the `ProjectId` type.
tutorialId :: RouteDuplex' String -> RouteDuplex' TutorialId
tutorialId = as show (map TutorialId <<< note "Cannot parse tutorial id" <<< fromString)

-- Prase a string into a Route
parseRoute :: String -> Either RouteError Route
parseRoute = parse routingCodec
