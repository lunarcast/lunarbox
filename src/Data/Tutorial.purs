module Lunarbox.Data.Tutorial where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Tuple (Tuple(..))
import Lunarbox.Data.Gist (GistId)
import Lunarbox.Data.ProjectId (ProjectId)
import Lunarbox.Data.Tab (Tab)

-- | Elements we can hide in the editor
-- | I'll add more soon
data EditorElement
  = Tab Tab

derive instance genericEditorElement :: Generic EditorElement _

instance encodeJsonEditorElement :: EncodeJson EditorElement where
  encodeJson = genericEncodeJson

instance decodeJsonEditorElement :: DecodeJson EditorElement where
  decodeJson = genericDecodeJson

-- | Id used to identify tutorials
newtype TutorialId
  = TutorialId Int

derive instance eqTutorialId :: Eq TutorialId

derive instance ordTutorialId :: Ord TutorialId

derive newtype instance showTutorialId :: Show TutorialId

derive newtype instance encodeJsonTutorialId :: EncodeJson TutorialId

derive newtype instance decodeJsonTutorialId :: DecodeJson TutorialId

-- | Type edited by the user visually
type TutorialSpec
  = { name :: String
    , base :: UserProject
    , solution :: UserProject
    , content :: GistId
    }

-- | The actual data structure for the tutorials
type Tutorial
  = { name :: String
    , base :: ProjectId
    , solution :: ProjectId
    , content :: GistId
    }

newtype UserProject
  = UserProject (Tuple String ProjectId)

derive instance eqUserProject :: Eq UserProject

instance showUserProject :: Show UserProject where
  show (UserProject (Tuple name _)) = name

instance semigroupUserProject :: Semigroup UserProject where
  append a b = b
