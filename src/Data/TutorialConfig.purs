module Lunarbox.Data.TutorialConfig where

import Prelude
import Data.Argonaut (decodeJson, jsonParser)
import Data.Either (Either(..), note)
import Data.Map as Map
import Data.Traversable (for)
import Lunarbox.Data.Gist (Gist(..), GistFiles(..))

-- | A step will be displayed as a modal in the editor while doing the tutorial
type TutorialStep
  = { title :: String
    , content :: String
    }

-- | Config made by an admin about what steps a tutorial has
type TutorialConfig
  = { steps :: Array TutorialStep
    }

-- | Basically grouped step data together from the github gist config
type TutorialSteps
  = { config :: TutorialConfig
    , steps :: Array TutorialStep
    }

-- | Get the tutorial steps from a github gist
getTutorialSteps :: Gist -> Either String TutorialSteps
getTutorialSteps (Gist { files: GistFiles files }) = do
  file <- note "Missing main.json file!" $ Map.lookup "main.json" files
  config :: TutorialConfig <- decodeJson =<< jsonParser file.content
  steps <-
    for config.steps \{ content, title } -> do
      slide <- note ("Missing slide: " <> content) $ Map.lookup content files
      pure
        { content: slide.content
        , title
        }
  Right { config, steps }
