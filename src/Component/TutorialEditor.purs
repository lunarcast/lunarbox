module Lunarbox.Component.TutorialEditor
  ( component
  ) where

import Prelude
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen (get, gets, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Capability.Resource.Project (class ManageProjects, getProjects)
import Lunarbox.Capability.Resource.Tutorial (class ManageTutorials, deleteTutorial, saveTutorial)
import Lunarbox.Component.Error (error)
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Loading (loading)
import Lunarbox.Component.Typeahead as TA
import Lunarbox.Component.Utils (className)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Data.Tutorial (TutorialId, TutorialSpec, UserProject(..))
import Lunarbox.Form.Field (customFormField)
import Lunarbox.Form.Field as Field
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
  | Delete
  | Init

component ::
  forall m.
  MonadAff m =>
  ManageProjects m =>
  ManageTutorials m =>
  Navigate m =>
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
    HandleTutorial
      { name
    , base: UserProject (Tuple _ base)
    , solution: UserProject (Tuple _ solution)
    } -> do
      { id } <- get
      response <-
        saveTutorial id
          { name
          , base
          , solution
          , steps: []
          , hiddenElements: []
          }
      case response of
        Left err -> modify_ _ { projects = Failure err }
        _ -> pure unit
    Delete -> do
      id <- gets _.id
      deleteTutorial id
        >>= case _ of
            Left err -> modify_ _ { projects = Failure err }
            _ -> navigate Projects
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
                , HH.button
                    [ className "tutorial-editor__delete"
                    , HE.onClick $ const $ Just Delete
                    ]
                    [ icon "delete"
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
      , solution :: f V.FormError (Maybe UserProject) UserProject
      )
  )

derive instance newtypeTutorialForm :: Newtype (TutorialForm r f) _

-- | Input to describe what options the typeahead in the form has
type FormInput
  = { projects :: Array UserProject }

data FormAction
  = HandleTypeahead UserProjectTypeahead (TA.Message Maybe UserProject)

-- | Different typeaheads used to select projects
data UserProjectTypeahead
  = Base
  | Solution

derive instance eqUserProjectTypeahead :: Eq UserProjectTypeahead

derive instance ordUserProjectTypeahead :: Ord UserProjectTypeahead

-- | Form child component types
type FormChildSlots
  = ( typeahead :: TA.Slot Maybe UserProject UserProjectTypeahead )

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
          , base: V.exists
          , solution: V.exists
          }
      , initialInputs: Nothing
      }

  handleAction = case _ of
    HandleTypeahead slot (TA.SelectionsChanged new) -> case slot of
      Base -> eval $ F.setValidate proxies.base new
      Solution -> eval $ F.setValidate proxies.solution new
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
          , customFormField proxies.base form
              $ singleTypeahead Base
                  { placeholder: "My awesome base project"
                  , items: projects
                  }
          , customFormField proxies.base form
              $ singleTypeahead Solution
                  { placeholder: "My awesome solution"
                  , items: projects
                  }
          , Field.submit "Save"
          ]
      ]

  singleTypeahead slot input = HH.slot TA._typeahead slot (Select.component TA.input TA.single) input handler
    where
    handler = Just <<< F.injAction <<< HandleTypeahead slot
