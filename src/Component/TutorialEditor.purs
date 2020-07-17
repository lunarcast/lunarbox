module Lunarbox.Component.TutorialEditor where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Editor.Node.NodeId (NodeId(..))
import Lunarbox.Data.Tutorial (TutorialId, TutorialSpec)
import Lunarbox.Form.Field as Field
import Lunarbox.Form.Validation as V

type Input
  = { id :: TutorialId
    }

data HandleAction
  = HandleTutorial TutorialSpec

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Input Void m
component =
  H.mkComponent
    { initialState: identity
    , render: const render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            }
    }
  where
  handleAction = case _ of
    HandleTutorial tut -> pure unit

  render =
    HH.section_
      [ HH.slot F._formless unit formComponent unit (Just <<< HandleTutorial)
      ]

-- Formless form for tutorials
newtype TutorialForm r f
  = TutorialForm
  ( r
      ( name :: f V.FormError String String
      , base :: f V.FormError String NodeId
      )
  )

derive instance newtypeTutorialForm :: Newtype (TutorialForm r f) _

formComponent ::
  forall m.
  MonadAff m =>
  F.Component TutorialForm (Const Void) () Unit TutorialSpec m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleEvent = F.raiseResult
        }
  where
  formInput _ =
    { validators:
      TutorialForm
        { name: F.noValidation
        , base: F.hoistFnE_ (Right <<< NodeId)
        }
    , initialInputs: Nothing
    }

  proxies = F.mkSProxies (F.FormProxy :: _ TutorialForm)

  render { form } =
    HH.div [ className "form" ]
      [ HH.div_
          [ Field.input proxies.name form
              [ HP.placeholder "Name"
              , HP.type_ HP.InputText
              ]
          , Field.submit "Save"
          ]
      ]
