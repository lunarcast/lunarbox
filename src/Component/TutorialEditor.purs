module Lunarbox.Component.TutorialEditor where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen (modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProjects)
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Typeahead as TA
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Tutorial (TutorialId, TutorialSpec, UserProject(..))
import Lunarbox.Form.Field as Field
import Lunarbox.Form.Validation (FormError(..))
import Lunarbox.Form.Validation as V
import Network.RemoteData (RemoteData(..))
import Record as Record
import Select as Select

type Input r
  = ( id :: TutorialId
    | r
    )

type State
  = { 
    | Input
      ( projects :: RemoteData String (Array UserProject)
      )
    }

data HandleAction
  = HandleTutorial TutorialSpec
  | Init

component ::
  forall m.
  MonadAff m =>
  ManageProjects m =>
  H.Component HH.HTML (Const Void) { | Input () } Void m
component =
  H.mkComponent
    { initialState:
      Record.merge
        { projects: NotAsked
        }
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction = case _ of
    HandleTutorial tut -> pure unit
    Init -> do
      result <- getProjects
      let
        projects = case result of
          Right { userProjects } -> Success $ mkProject <$> userProjects
            where
            mkProject { id, name } = UserProject $ Tuple name id
          Left err -> Failure err
      modify_ _ { projects = projects }

  render { projects } = case projects of
    Failure err -> error err
    Success projects' ->
      HH.div [ className "tutorial-editor" ]
        [ HH.div
            [ className "tutorial-editor__main" ]
            [ HH.header [ className "tutorial-editor__header" ]
                [ HH.h1 [ className "tutorial-editor__title" ]
                    [ HH.text "Edit tutorial"
                    ]
                ]
            , HH.slot F._formless unit formComponent
                { projects: projects' }
                (Just <<< HandleTutorial)
            ]
        ]
    _ -> loading

-- Formless form for tutorials
newtype TutorialForm r f
  = TutorialForm
  ( r
      ( name :: f V.FormError String String
      , base :: f V.FormError (Maybe UserProject) UserProject
      )
  )

derive instance newtypeTutorialForm :: Newtype (TutorialForm r f) _

-- | Input to describe what options the typeahead in the form has
type FormInput
  = { projects :: Array UserProject }

data FormAction
  = HandleTypeahead (TA.Message Maybe UserProject)

-- | Form child component types
type FormChildSlots
  = ( typeahead :: TA.Slot Maybe UserProject Unit )

formComponent ::
  forall m.
  MonadAff m =>
  F.Component TutorialForm (Const Void) FormChildSlots FormInput TutorialSpec m
formComponent =
  F.component formInput
    $ F.defaultSpec
        { render = render
        , handleEvent = handleEvent
        , handleAction = handleAction
        }
  where
  handleEvent = F.raiseResult

  formInput =
    Record.merge
      { validators:
        TutorialForm
          { name: F.noValidation
          , base: F.hoistFnE_ $ maybe (Left Required) Right
          }
      , initialInputs: Nothing
      }

  handleAction = case _ of
    HandleTypeahead (TA.SelectionsChanged new) -> do
      eval $ F.setValidate proxies.base new
    where
    eval act = F.handleAction handleAction handleEvent act

  proxies = F.mkSProxies (F.FormProxy :: _ TutorialForm)

  render { form, projects } =
    HH.div [ className "form" ]
      [ HH.div_
          [ Field.input proxies.name form
              [ HP.placeholder "Name"
              , HP.type_ HP.InputText
              ]
          , singleTypeahead unit
              { placeholder: "My awesome project"
              , items: projects
              }
          , Field.submit "Save"
          ]
      ]

  singleTypeahead slot input = HH.slot TA._typeahead slot (Select.component TA.input TA.single) input handler
    where
    handler = Just <<< F.injAction <<< HandleTypeahead
