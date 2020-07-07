module Lunarbox.Component.Editor
  ( component
  , Action(..)
  , Output(..)
  , PendingConnectionAction(..)
  , EditorState
  , ChildSlots
  ) where

import Prelude
import Control.Monad.Reader (class MonadReader, asks)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Argonaut (Json)
import Data.Array ((!!))
import Data.Foldable (for_, traverse_)
import Data.List ((:))
import Data.Lens (over, set, view)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable) as Set
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..), Component, HalogenM, RefLabel(..), Slot, SubscriptionId, defaultEval, fork, getHTMLElementRef, mkComponent, mkEval, query, raise, subscribe', tell)
import Halogen.HTML (lazy, lazy2)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onKeyUp, onMouseUp, onValueInput)
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Lunarbox.Capability.Navigate (class Navigate, navigate)
import Lunarbox.Component.Editor.Add as AddC
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Modal as Modal
import Lunarbox.Component.Switch (switch)
import Lunarbox.Component.Utils (className, maybeElement, whenElem)
import Lunarbox.Config (Config, _autosaveInterval)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.TypeError (printError)
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Save (stateToJson)
import Lunarbox.Data.Editor.State (State, Tab(..), _atInputCount, _currentFunction, _currentTab, _isAdmin, _isExample, _isVisible, _name, _nodeSearchTerm, _panelIsOpen, compile, createConnection, createNode, deleteFunction, evaluate, functionExists, generateUnconnectableInputs, generateUnconnectableOutputs, initializeFunction, preventDefaults, removeConnection, searchNode, setCurrentFunction, setRuntimeValue, tabIcon, tryCompiling, updateAll, withCurrentGeometries)
import Lunarbox.Data.Graph (wouldCreateCycle)
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Data.Set (toNative) as Set
import Lunarbox.Foreign.Render (setUnconnectableInputs, setUnconnectableOutputs)
import Lunarbox.Foreign.Render as Native
import Web.Event.Event (Event, preventDefault, stopPropagation)
import Web.Event.Event as Event
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (focus)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data Action
  = Init
  | HandleKey SubscriptionId KeyboardEvent
  | ChangeTab Tab
  | CreateFunction FunctionName
  | SelectFunction FunctionName
  | CreateNode FunctionName
  | StartFunctionCreation
  | RemoveConnection NodeId Int
  | SetRuntimeValue FunctionName NodeId RuntimeValue
  | TogglePanel
  | ChangeInputCount FunctionName Int
  | SetName String
  | SetExample Boolean
  | SearchNodes String
  | SetVisibility Boolean
  | HandleAddPanelKeyPress KeyboardEvent
  | DeleteFunction FunctionName
  | Autosave Json
  | PreventDefaults Event
  | Navigate Route
  | LoadScene
  | Rerender
  -- Handle foreign actions bubbled by the Scene component
  | CreateConnection NodeId NodeId Int
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | HandleConnectionConfirmation PendingConnectionAction

data Output
  = Save Json

type ChildSlots
  = ( tree :: Slot TreeC.Query TreeC.Output Unit
    , scene :: Slot Scene.Query Scene.Output Unit
    , pendingConnection :: Slot Modal.Query (Modal.Output PendingConnectionAction) Unit
    )

-- Shorthand for manually passing the types of the actions and child slots
type EditorState m
  = State Action ChildSlots m

-- We use this to automatically focus on the correct element when pressing S
searchNodeInputRef :: RefLabel
searchNodeInputRef = RefLabel "search node"

-- We need this to only trigger events when in focus
searchNodeClassName :: String
searchNodeClassName = "node-search__input"

-- | Actions which can be triggered from the connection confirmation modal.
data PendingConnectionAction
  = AutofixConnection
  | CancelConnecting
  | ProceedConnecting

-- | The actual config for how to display the connection confirmation modal
pendingConnectionModal :: forall m. Modal.InputType PendingConnectionAction m
pendingConnectionModal =
  { id: "confirm-connection"
  , title: "Confirm connection"
  , content: HH.text "Connecting those nodes would change the type of this function creating a type error."
  , buttons:
    [ { text: "Autofix"
      , primary: true
      , value: AutofixConnection
      }
    , { text: "Continue"
      , primary: false
      , value: ProceedConnecting
      }
    , { text: "Cancel"
      , primary: false
      , value: CancelConnecting
      }
    ]
  , onClose: CancelConnecting
  }

component :: forall m q. MonadAff m => MonadEffect m => MonadReader Config m => Navigate m => Component HH.HTML q (EditorState m) Output m
component =
  mkComponent
    { initialState: compile <<< loadPrelude <<< identity
    , render
    , eval:
      mkEval
        $ defaultEval
            { handleAction = handleAction
            , initialize = Just Init
            }
    }
  where
  handleAction :: Action -> HalogenM (EditorState m) Action ChildSlots Output m Unit
  handleAction = case _ of
    Init -> do
      window <- liftEffect Web.window
      document <- liftEffect $ Web.document window
      -- Stuff which we need to run at the start
      handleAction LoadScene
      -- Register keybindings
      subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keydown
          (HTMLDocument.toEventTarget document)
          (map (HandleKey sid) <<< KE.fromEvent)
      void <<< fork <<< handleAction <<< Autosave <<< stateToJson =<< get
    HandleKey sid event
      -- TODO: readd this with the new foreign system
      -- | KE.key event == "Delete" || (KE.ctrlKey event && KE.key event == "Backspace") -> do
      --   modify_ deleteSelection
      | KE.ctrlKey event && KE.key event == "b" -> do
        handleAction TogglePanel
      | KE.ctrlKey event && KE.key event == "i" -> handleAction $ CreateNode $ FunctionName "input"
      | KE.key event == "s" -> do
        let
          targetInput =
            HTMLInputElement.fromEventTarget
              =<< Event.target (KE.toEvent event)
        when (isNothing targetInput) do
          { currentTab, panelIsOpen } <- get
          when (currentTab /= Add || not panelIsOpen) do
            modify_ $ set _panelIsOpen true <<< set _currentTab Add
            void $ query (SProxy :: _ "scene") unit $ tell Scene.HandleResize
          liftEffect $ preventDefault $ KE.toEvent event
          getHTMLElementRef searchNodeInputRef >>= traverse_ (liftEffect <<< focus)
      | otherwise -> pure unit
    HandleAddPanelKeyPress event
      | KE.key event == "Enter" -> do
        sortedFunctions <- gets searchNode
        currentFunction <- gets $ view _currentFunction
        functionGraph <- gets $ toGraph <<< _.project
        let
          bestMatch = sortedFunctions !! 0
        when
          ( maybe false not $ wouldCreateCycle <$> bestMatch <*> Just currentFunction
              <*> Just functionGraph
          )
          $ for_ bestMatch (handleAction <<< CreateNode)
      | KE.ctrlKey event && KE.shiftKey event && KE.key event == " " -> do
        inputElement <- getHTMLElementRef searchNodeInputRef
        for_ (inputElement >>= HTMLInputElement.fromHTMLElement) \elem -> do
          state <- get
          inputValue <- liftEffect $ HTMLInputElement.value elem
          let
            inputFunctionName = FunctionName inputValue

            exists = functionExists inputFunctionName state
          if inputValue /= "" && (not exists) then do
            initializeFunction inputFunctionName state >>= put
          else
            if exists then
              handleAction $ SelectFunction inputFunctionName
            else
              pure unit
      | KE.ctrlKey event && KE.key event == " " -> do
        sortedFunctions <- gets searchNode
        for_ (sortedFunctions !! 0) $ handleAction <<< SelectFunction
      | otherwise -> liftEffect $ stopPropagation $ KE.toEvent event
    CreateNode name -> do
      createNode name
      handleAction Rerender
    TogglePanel -> do
      modify_ $ over _panelIsOpen not
      void $ query (SProxy :: _ "scene") unit $ tell Scene.HandleResize
    ChangeTab newTab -> do
      oldTab <- gets $ view _currentTab
      panelWasOpen <- gets $ view _panelIsOpen
      if (oldTab == newTab) then
        handleAction TogglePanel
      else do
        modify_ $ set _currentTab newTab <<< set _panelIsOpen true
        void $ query (SProxy :: _ "scene") unit $ tell Scene.HandleResize
    CreateFunction name -> do
      get >>= initializeFunction name >>= put
      handleAction LoadScene
    SelectFunction name -> do
      modify_ $ setCurrentFunction name
      handleAction LoadScene
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    RemoveConnection id index -> do
      modify_ $ removeConnection $ Tuple id index
      void updateAll
      handleAction Rerender
    SetRuntimeValue functionName nodeId runtimeValue -> do
      modify_ $ setRuntimeValue functionName nodeId runtimeValue
    ChangeInputCount function amount -> do
      modify_ $ set (_atInputCount function) $ Just amount
    SetName name -> modify_ $ set _name name
    SetVisibility isVisible -> do
      modify_ $ set _isVisible isVisible
    SetExample isExample -> do
      -- We only allow editing the example status when we are an admine
      isAdmin <- gets $ view _isAdmin
      when isAdmin $ modify_ $ set _isExample isExample
    SearchNodes input -> modify_ $ set _nodeSearchTerm input
    DeleteFunction name -> modify_ $ deleteFunction name
    Autosave oldState -> do
      interval <- asks $ view _autosaveInterval
      liftAff $ delay interval
      name <- gets $ view _name
      if String.length name >= 2 then do
        { errors, expression, typeMap } <- get
        for_ errors $ printString <<< printError show
        newState <- gets stateToJson
        raise $ Save newState
        handleAction $ Autosave newState
      else
        handleAction $ Autosave oldState
    PreventDefaults event -> preventDefaults event
    Navigate route -> navigate route
    Rerender -> void $ query (SProxy :: SProxy "scene") unit $ tell $ Scene.Rerender
    LoadScene -> do
      updateAll
        >>= traverse_
            ( void
                <<< query (SProxy :: _ "scene") unit
                <<< tell
                <<< Scene.LoadScene
            )
    CreateConnection from toId toIndex -> do
      state <- createConnection from toId toIndex <$> get
      let
        { expression, typeMap, errors } = tryCompiling state
      case errors of
        [] -> do
          put $ evaluate $ state { expression = expression, typeMap = typeMap, errors = errors }
          printString "Created connection with no errors"
          void updateAll
        _ -> do
          modify_
            _
              { pendingConnection =
                Just
                  { from
                  , toId
                  , toIndex
                  , expression
                  , errors
                  , typeMap
                  }
              }
          handleAction (HandleConnectionConfirmation ProceedConnecting)
          void $ query (SProxy :: SProxy "pendingConnection") unit $ tell Modal.Open
    SelectInput id index ->
      void
        $ withCurrentGeometries \cache -> do
            state <- get
            liftEffect $ setUnconnectableOutputs cache
              $ Set.toNative
              $ generateUnconnectableOutputs (Tuple id index) state
            handleAction Rerender
    SelectOutput id ->
      void
        $ withCurrentGeometries \cache -> do
            state <- get
            liftEffect $ setUnconnectableInputs cache $ Set.toNative
              $ generateUnconnectableInputs id state
            handleAction Rerender
    HandleConnectionConfirmation CancelConnecting -> modify_ _ { pendingConnection = Nothing }
    HandleConnectionConfirmation other -> do
      p <- gets _.pendingConnection
      gets _.pendingConnection
        >>= traverse_ \{ from, toId, toIndex, errors, expression, typeMap } -> case other of
            ProceedConnecting -> do
              state <- get
              put
                $ createConnection from toId toIndex
                $ state
                    { pendingConnection = Nothing
                    , errors = errors
                    , typeMap = typeMap
                    , expression = expression
                    }
              void updateAll
              handleAction Rerender
            AutofixConnection -> modify_ _ { pendingConnection = Nothing }
            -- WARNING: this should never happen
            _ -> pure unit

  handleTreeOutput :: TreeC.Output -> Maybe Action
  handleTreeOutput = case _ of
    TreeC.CreatedFunction name -> Just $ CreateFunction name
    TreeC.SelectedFunction name -> Just $ SelectFunction name

  handleSceneOutput :: Scene.Output -> Maybe Action
  handleSceneOutput (Scene.BubbleForeignAction action) = case action of
    Native.CreateConnection from toId toIndex -> Just $ CreateConnection from toId toIndex
    Native.SelectOutput id -> Just $ SelectOutput id
    Native.SelectInput id index -> Just $ SelectInput id index
    Native.DeleteConnection id index -> Just $ RemoveConnection id index
    _ -> Nothing

  sidebarIcon activeTab current =
    HH.div
      [ classes $ ClassName <$> [ "editor__activity" ] <> (guard isActive $> "editor__activity--active")
      , onClick $ const $ Just $ ChangeTab current
      ]
      [ icon $ tabIcon current ]
    where
    isActive = current == activeTab

  tabs currentTab =
    [ icon Settings
    , icon Add
    , icon Tree
    , icon Problems
    ]
    where
    icon = sidebarIcon currentTab

  mkPanel :: _
  mkPanel { title, actions, content, footer, header } =
    [ HH.header [ className "panel__header" ]
        [ HH.div [ className "panel__title-container" ]
            $ [ HH.h1 [ className "panel__title" ] [ HH.text title ]
              ]
            <> ( ( \action ->
                    HH.div [ className "panel__action", onClick $ const action.onClick ]
                      [ icon action.icon
                      ]
                )
                  <$> actions
              )
        , maybeElement header identity
        ]
    , HH.main [ className "panel__content" ]
        content
    , maybeElement footer \inner -> HH.footer [ className "panel__footer" ] [ inner ]
    ]

  panel :: State Action ChildSlots m -> Array (HH.ComponentHTML Action ChildSlots m)
  panel { currentTab, project, currentFunction, functionData, typeMap, inputCountMap, name, isExample, isVisible, isAdmin, nodeSearchTerm } = case currentTab of
    Settings ->
      mkPanel
        { title: "Project settings"
        , actions: []
        , footer: Nothing
        , header: Nothing
        , content:
          [ HH.div
              [ className "setting" ]
              [ HH.div [ className "setting__label" ] [ HH.text "Name:" ]
              , HH.input
                  [ HP.value name
                  , HP.placeholder "Project name"
                  , className "setting__text-input"
                  , onValueInput $ Just <<< SetName
                  ]
              ]
          , HH.div [ className "setting" ]
              [ HH.div [ className "setting__label" ] [ HH.text "Visibility:" ]
              , HH.div [ className "setting__switch-input" ]
                  [ switch { checked: isVisible, round: true } (Just <<< SetVisibility)
                  ]
              ]
          , whenElem isAdmin \_ ->
              HH.div [ className "project-setting" ]
                [ HH.div [ className "setting__label" ] [ HH.text "Example:" ]
                , HH.div [ className "setting__switch-input" ]
                    [ switch { checked: isExample, round: true } (Just <<< SetExample)
                    ]
                ]
          ]
        }
    Tree ->
      mkPanel
        { title: "Explorer"
        , actions: [ { icon: "note_add", onClick: Just StartFunctionCreation } ]
        , footer: Nothing
        , header: Nothing
        , content:
          [ HH.slot (SProxy :: _ "tree") unit TreeC.component
              { functions:
                currentFunction
                  : ( Set.toUnfoldable $ Map.keys $ onlyEditable currentFunction project
                    )
              , selected: currentFunction
              }
              handleTreeOutput
          ]
        }
    Add ->
      mkPanel
        { title: "Add node"
        , actions: []
        , header:
          Just
            $ flip lazy nodeSearchTerm \nodeSearchTerm' ->
                HH.div [ className "node-search" ]
                  [ HH.input
                      [ HP.placeholder "Search nodes. Eg: add, greater than..."
                      , HP.value nodeSearchTerm'
                      , HP.ref searchNodeInputRef
                      , className searchNodeClassName
                      , onKeyUp $ Just <<< HandleAddPanelKeyPress
                      , onValueInput $ Just <<< SearchNodes
                      ]
                  , icon "search"
                  ]
        , footer:
          Just
            $ HH.button
                [ className "unselectable node-panel__create-input-button"
                , onClick $ const $ Just $ CreateNode $ FunctionName "input"
                ]
                [ HH.text "Create input node"
                ]
        , content:
          [ lazy2 AddC.add
              { project
              , currentFunction
              , functionData
              , typeMap
              , inputCountMap
              , nodeSearchTerm
              }
              { edit: Just <<< SelectFunction
              , addNode: Just <<< CreateNode
              , changeInputCount: (Just <<< _) <<< ChangeInputCount
              , delete: Just <<< DeleteFunction
              }
          ]
        }
    Problems ->
      mkPanel
        { title: "Problems"
        , actions: []
        , footer: Nothing
        , header: Nothing
        , content: [ HH.text "unimplemented" ]
        }

  scene :: HH.ComponentHTML Action ChildSlots m
  scene = HH.slot (SProxy :: _ "scene") unit Scene.component unit handleSceneOutput

  logoElement =
    HH.img
      [ HP.src "https://cdn.discordapp.com/attachments/672889285438865453/708081533151477890/favicon.png"
      , HP.alt "Lunarbox logo"
      , className "editor__logo"
      , onMouseUp $ const $ Just $ Navigate Home
      ]

  render :: State Action ChildSlots m -> HH.ComponentHTML Action ChildSlots m
  render state@{ currentTab, panelIsOpen } =
    HH.div [ className "editor" ]
      [ HH.div [ className "editor__activity-bar" ]
          $ tabs currentTab
          <> pure logoElement
      , HH.div
          [ classes $ ClassName
              <$> [ "panel" ]
              <> (guard panelIsOpen $> "panel--open")
          ]
          $ panel state
      , HH.div [ className "scene" ]
          [ scene
          ]
      , HH.slot (SProxy :: SProxy "pendingConnection") unit Modal.component
          pendingConnectionModal
          handleConnectionConfirmation
      ]
    where
    handleConnectionConfirmation = Just <<< HandleConnectionConfirmation <<< unwrap
