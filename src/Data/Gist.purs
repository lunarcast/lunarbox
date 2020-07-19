module Lunarbox.Data.Gist where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Foreign.Object (Object)

-- | Data about a particular file in a gist
type GistFile
  = { filename :: String
    , type :: String
    , language :: String
    , size :: Int
    , truncated :: Boolean
    , content :: String
    , raw_url :: String
    }

-- | I made a separate newtype so I can deocde this properly
newtype GistFiles
  = GistFiles (Map String GistFile)

instance decodeJsonGistFiles :: DecodeJson GistFiles where
  decodeJson json = do
    obj :: Object GistFile <- decodeJson json
    pure $ GistFiles $ Map.fromFoldableWithIndex obj

newtype Gist
  = Gist
  { url :: String
  , id :: GistId
  , public :: Boolean
  , description :: String
  , files :: GistFiles
  }

derive newtype instance decodeJsonGist :: DecodeJson Gist

-- | Id we can use to retrive a particular gist
newtype GistId
  = GistId String

instance showGistId :: Show GistId where
  show (GistId a) = a

derive instance newtypeGistId :: Newtype GistId _

derive instance eqGistId :: Eq GistId

derive newtype instance decodeJsonGistId :: DecodeJson GistId
