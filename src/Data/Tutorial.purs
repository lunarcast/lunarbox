module Lunarbox.Data.Tutorial where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.List.Types (List(..), NonEmptyList, (:))
import Data.Validation.Semigroup (V, invalid)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue(..))
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
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

-- | Different ways of informing users about stuff 
-- | (will add more of them in the future)
newtype TutorialStep
  = TextBlock String

derive newtype instance encodeJsonTutorialStep :: EncodeJson TutorialStep

derive newtype instance decodeJsonTutorialStep :: DecodeJson TutorialStep

-- | A test case for an user defined function 
newtype TutorialTest
  = Test
  { inputs :: List RuntimeValue
  , output :: RuntimeValue
  }

derive newtype instance encodeJsonTutorialTest :: EncodeJson TutorialTest

derive newtype instance decodeJsonTutorialTest :: DecodeJson TutorialTest

-- | Id used to identify tutorials
newtype TutorialId
  = TutorialId Int

derive instance eqTutorialId :: Eq TutorialId

derive instance ordTutorialId :: Ord TutorialId

derive newtype instance showTutorialId :: Show TutorialId

derive newtype instance encodeJsonTutorialId :: EncodeJson TutorialId

derive newtype instance decodeJsonTutorialId :: DecodeJson TutorialId

-- | The actual data structure for the tutorials
type Tutorial r
  = { name :: String
    , id :: TutorialId
    , base :: NodeId
    , requires :: Array TutorialId
    , steps :: Array TutorialStep
    , hiddenElements :: Array EditorElement
    , tests :: Array TutorialTest
    | r
    }

type TutorialFields
  = Tutorial ()

type TutorialWithMetadata
  = Tutorial ( completed :: Boolean )

-- | Possible errors we can get by validating a tutorial
data TutorialValidationError
  = NonEqual RuntimeValue RuntimeValue
  | ExpectedFunction RuntimeValue RuntimeValue

-- | Validate a test
validateTest :: RuntimeValue -> TutorialTest -> V (NonEmptyList TutorialValidationError) Unit
validateTest result (Test { inputs: Nil, output })
  | result == output = pure unit
  | otherwise = invalid $ pure $ NonEqual result output

validateTest (Function call) (Test { inputs: head : inputs, output }) =
  validateTest
    (call head)
    (Test { inputs, output })

validateTest nonFunction (Test { output }) = invalid $ pure $ ExpectedFunction nonFunction output

-- | Validate a tutorial
validateTutorial :: forall r. RuntimeValue -> Tutorial r -> V (NonEmptyList TutorialValidationError) Unit
validateTutorial main { tests } = fold $ validateTest main <$> tests

-- | Type edited by the user visually
type TutorialSpec
  = { name :: String
    , base :: NodeId
    }
