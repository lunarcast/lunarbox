module Lunarbox.Component.Editor.Tree where

import Prelude
import Control.MonadZero (guard)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Effect.Class (class MonadEffect)
import Halogen (Component, HalogenM, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events (onInput, onKeyUp)
import Halogen.HTML.Properties as Hp
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (StaticHtml, container)
import Web.UIEvent.InputEvent as IE
import Web.UIEvent.KeyboardEvent as KE

type State
  = { functions :: List String
    , nextName :: Maybe String
    }

data Action
  = CreateFunction
  | UpdateNextName IE.InputEvent

data Query a
  = StartCreation a

type ChildSlots
  = ()

type Input
  = List String

data Output
  = CreatedFunction String

component :: forall m. MonadEffect m => Component HH.HTML Query Input Output m
component =
  mkComponent
    { initialState: (\input -> { functions: input, nextName: Nothing })
    , render
    , eval:
        mkEval
          $ defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  handleAction :: Action -> HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    UpdateNextName event -> do
      { nextName } <- get
      when (isJust nextName) $ modify_ (_ { nextName = (_ <> IE.data_ event) <$> nextName })
    CreateFunction -> do
      { nextName, functions } <- get
      case nextName of
        Just name -> do
          modify_ (_ { nextName = Nothing, functions = functions <> pure name })
          raise $ CreatedFunction name
        Nothing -> pure unit

  handleQuery :: forall a. Query a -> HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    StartCreation a -> do
      modify_ (_ { nextName = Just "" })
      pure (Just a)

  displayFunction :: forall a b. StaticHtml String a b
  displayFunction name = HH.div_ [ icon "code", HH.text name ]

  render :: forall a. State -> HTML a Action
  render { functions, nextName } = container "functions" (existingFunctions <> newFunctionTextBox)
    where
    existingFunctions = List.toUnfoldable $ displayFunction <$> functions

    newFunctionTextBox = case nextName of
      Just name ->
        [ container "create-function-input-container"
            [ icon "code"
            , HH.input
                [ onInput $ map UpdateNextName <<< IE.fromEvent
                , onKeyUp \event -> do
                    guard (KE.key event == "Enter")
                    pure CreateFunction
                , Hp.autofocus true
                , Hp.id_ "create-function-input"
                ]
            ]
        ]
      Nothing -> []
