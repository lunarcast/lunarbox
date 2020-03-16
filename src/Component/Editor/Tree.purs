module Lunarbox.Component.Editor.Tree where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (traverse_)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component, HalogenM, RefLabel(..), defaultEval, get, getHTMLElementRef, mkComponent, mkEval, modify_, raise)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onBlur, onClick, onKeyUp)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (StaticHtml, container)
import Lunarbox.Data.Project (FunctionName(..))
import Web.HTML.HTMLElement (blur, focus)
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.KeyboardEvent as KE

type State
  = { functions :: List FunctionName
    , creating :: Boolean
    , selected :: Maybe FunctionName
    }

data Action
  -- Uusally runs when the user presses enter while focused on the input box
  = CreateFunction
  -- If the used blurs out of the input it means he canceled the creation
  | CancelCreation
  -- This runs when the user clicks on a function
  | SelectFunction FunctionName

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
    { initialState: (\{ functions, selected } -> { functions, selected, creating: false })
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

  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    CancelCreation -> do
      modify_ (_ { creating = false })
    SelectFunction name -> do
      modify_ (_ { selected = Just name })
      raise $ SelectedFunction $ Just name
    CreateFunction -> do
      { creating, functions } <- get
      -- if creating would be false we would get undefined behavior
      when creating do
        maybeElement <- getHTMLElementRef inputRef
        -- move the element out of focus
        liftEffect $ traverse_ blur maybeElement
        -- this is here in case the selected element isnt an input element
        maybeElement >>= InputElement.fromHTMLElement
          # traverse_ \element -> do
              -- get the text in the input box
              name <- liftEffect $ InputElement.value element
              -- this saves the new function in the list
              modify_ (_ { creating = false, functions = functions <> (pure $ FunctionName name) })
              -- this notifies the parent element we just created a new function
              -- the parent ususally has to add the function to the graph
              raise $ CreatedFunction $ FunctionName name

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
  render { functions, creating, selected } = container "functions" (existingFunctions <> newFunctionTextBox)
    where
    -- this is the html for the list of existing functions
    existingFunctions = List.toUnfoldable $ (displayFunction selected) <$> functions

    -- this is a list which may contain the input box for creating new functions
    newFunctionTextBox =
      guard creating
        $> container "create-function-input-container"
            [ icon "code"
            , HH.input
                [ HP.id_ "create-function-input"
                , onKeyUp \event -> do
                    -- when the user presses enter we create the function
                    guard (KE.key event == "Enter")
                    pure CreateFunction
                -- if the user clicks outside the input we can cancel the creation
                , onBlur $ const $ Just $ CancelCreation
                -- this will only work for the first function creation, but it's still good to haves
                , HP.autofocus true
                -- the ref is necessary to solve focus issues
                , HP.ref inputRef
                ]
            ]
