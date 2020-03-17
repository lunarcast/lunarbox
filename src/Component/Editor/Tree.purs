module Lunarbox.Component.Editor.Tree where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (traverse_, find)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, mkComponent, mkEval, modify_, raise)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onBlur, onClick, onInput, onKeyUp)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Tooltip (maybeTooltip)
import Lunarbox.Component.Utils (StaticHtml, container)
import Lunarbox.Data.Project (FunctionName(..))
import Web.HTML.HTMLElement (blur, focus)
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.KeyboardEvent as KE

data ValidationError
  = Duplicate String
  | Empty

validationErrorToHtml :: forall a b. ValidationError -> HTML a b
validationErrorToHtml (Duplicate s) = HH.span_ [ HH.text "Function ", HH.strong_ [ HH.text s ], HH.text " already exists." ]

validationErrorToHtml Empty = HH.text "Function names cannot be empty"

type State
  = { functions :: List FunctionName
    , creating :: Boolean
    , selected :: Maybe FunctionName
    , validationError :: Maybe ValidationError
    }

data Action
  -- Uusally runs when the user presses enter while focused on the input box
  = CreateFunction
  -- If the used blurs out of the input it means he canceled the creation
  | CancelCreation
  -- This runs when the user clicks on a function
  | SelectFunction FunctionName
  -- This runs when the user types anything
  | ValidateFunctionName

data Query a
  -- This is a message from the parent meaning we can start the cretion process
  = StartCreation a

type ChildSlots
  = ()

type Input
  -- The initial function list and selected function
  = { functions :: List FunctionName
    , selected :: Maybe FunctionName
    }

data Output
  -- This notifies the parent when  new function was created
  = CreatedFunction
    FunctionName
  -- This notifies the parent when the selected function changed
  | SelectedFunction (Maybe FunctionName)

component :: forall m. MonadEffect m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: (\{ functions, selected } -> { functions, selected, creating: false, validationError: Nothing })
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  -- we need a ref to access the content of the input element
  inputRef :: RefLabel
  inputRef = RefLabel "create-function-input"

  validate :: HalogenM State Action ChildSlots Output m Unit
  validate = do
    -- this gives the the element from the dom if it exists
    maybeElement <- getHTMLElementRef inputRef
    -- this is here in case the selected element isnt an input element
    maybeElement >>= InputElement.fromHTMLElement
      # traverse_ \element -> do
          -- this gives us the inner value of the input box so we can validate it
          name <- liftEffect $ InputElement.value element
          { functions } <- get
          -- here we check if the validation fails and if it does we display the error
          if (Maybe.isJust $ (_ == FunctionName name) `find` functions) then
            modify_ (_ { validationError = Just $ Duplicate name })
          else
            if (name == "") then
              modify_ (_ { validationError = Just $ Empty })
            else
              modify_ (_ { validationError = Nothing })

  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    CancelCreation -> do
      when false $ modify_ (_ { creating = false })
    SelectFunction name -> do
      modify_ (_ { selected = Just name })
      raise $ SelectedFunction $ Just name
    ValidateFunctionName -> validate
    CreateFunction -> do
      -- validate in case the user pressed enter right away
      validate
      -- we get the data after vaildating to be sure the validationError is up to date
      { creating, functions, validationError } <- get
      -- if creating would be false we would get undefined behavior
      when creating do
        maybeElement <- getHTMLElementRef inputRef
        -- this is here in case the selected element isnt an input element
        maybeElement >>= InputElement.fromHTMLElement
          # traverse_ \element -> do
              -- get the text in the input box
              name <- liftEffect $ InputElement.value element
              let
                functionName = FunctionName name
              when (Maybe.isNothing validationError) do
                -- move the element out of focus
                liftEffect $ traverse_ blur maybeElement
                -- this saves the new function in the list
                -- we also automatically select the new function
                modify_ (_ { creating = false, functions = functions <> (pure functionName), selected = Just functionName })
                -- this notifies the parent element we just created a new function
                -- the parent ususally has to add the function to the graph
                raise $ CreatedFunction functionName
                -- we also need to make the parent select this function too
                raise $ SelectedFunction $ Just functionName

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    StartCreation a -> do
      modify_ (_ { creating = true })
      -- we need the input element to bring it in focus
      maybeElement <- getHTMLElementRef inputRef
      -- this brings the element in focus
      -- we need to do this so the use doesn't have to press tab or move the mouse to type
      liftEffect $ traverse_ focus maybeElement
      pure (Just a)

  -- renders an element in the list
  -- I'll have to update it when I'll add support for recursive functions
  displayFunction :: forall a. Maybe FunctionName -> StaticHtml FunctionName a Action
  displayFunction selected name =
    HH.div
      [ onClick $ const $ Just $ SelectFunction name
      , classes $ ClassName <$> ("selected" <$ guard (Just name == selected))
      , HP.id_ "function"
      ]
      [ icon "code", HH.text $ show name ]

  render :: forall a. State -> HTML a Action
  render { functions, creating, selected, validationError } = container "functions" (existingFunctions <> newFunctionTextBox)
    where
    -- this is the html for the list of existing functions
    existingFunctions = List.toUnfoldable $ (displayFunction selected) <$> functions

    -- this is a list which may contain the input box for creating new functions
    newFunctionTextBox =
      guard creating
        $> container "create-function-input-container"
            [ icon "code"
            , maybeTooltip (validationErrorToHtml <$> validationError)
                $ HH.input
                    [ HP.id_ "create-function-input"
                    , onKeyUp \event -> do
                        -- when the user presses enter we create the function
                        guard (KE.key event == "Enter")
                        pure CreateFunction
                    -- if the user clicks outside the input we can cancel the creation
                    , onBlur $ const $ Just CancelCreation
                    , onInput $ const $ Just ValidateFunctionName
                    -- this will only work for the first function creation, but it's still good to haves
                    , HP.autofocus true
                    -- the ref is necessary to solve focus issues
                    , HP.ref inputRef
                    -- we don't want autocomplete for function names
                    , HP.autocomplete false
                    ]
            ]
