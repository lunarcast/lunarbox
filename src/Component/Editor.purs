module Lunarbox.Component.Editor
  ( component
  , Action(..)
  , Output(..)
  , PendingConnectionAction(..)
  , NodeEditingAction(..)
  , ChildSlots
  ) where

import Prelude
import Control.Monad.Reader (class MonadReader, asks)
import Control.Monad.State (get, gets, modify_, put)
import Control.MonadZero (guard)
import Data.Argonaut (Json)
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (for_, traverse_)
import Data.Lens (_Just, is, over, preview, set, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isNothing, maybe)
import Data.Set (toUnfoldable) as Set
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
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
import Lunarbox.Component.Editor.Add as Add
import Lunarbox.Component.Editor.Add as AddC
import Lunarbox.Component.Editor.NodePreview as NodePreview
import Lunarbox.Component.Editor.Problems (problems, shouldRender)
import Lunarbox.Component.Editor.Scene as Scene
import Lunarbox.Component.Editor.Tree as TreeC
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Modal as Modal
import Lunarbox.Component.Switch (switch)
import Lunarbox.Component.Utils (className, maybeElement, whenElem)
import Lunarbox.Config (Config, _autosaveInterval)
import Lunarbox.Control.Monad.Effect (printString)
import Lunarbox.Data.Class.GraphRep (toGraph)
import Lunarbox.Data.Dataflow.Expression.Lint as LintError
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Dataflow.Runtime (RuntimeValue)
import Lunarbox.Data.Dataflow.Type as Type
import Lunarbox.Data.Dataflow.TypeError as TypeError
import Lunarbox.Data.Editor.DataflowFunction (DataflowFunction(..), _NativeFunction)
import Lunarbox.Data.Editor.EditNode as EditNode
import Lunarbox.Data.Editor.FunctionName (FunctionName(..))
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.Node (Node(..))
import Lunarbox.Data.Editor.Node.NodeDescriptor (onlyEditable)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Project as Project
import Lunarbox.Data.Editor.Save (stateToJson)
import Lunarbox.Data.Editor.State (MovementStep(..), State, Tab(..), _atFunctionData, _atInputCount, _atNode, _currentFunction, _currentTab, _function, _functions, _isAdmin, _isExample, _isVisible, _name, _nodeSearchTerm, _panelIsOpen, compile, createConnection, createNode, deleteFunction, deleteNode, evaluate, functionExists, generateUnconnectableInputs, generateUnconnectableOutputs, getFunctionColorMap, getMaxInputs, getNodeType, initializeFunction, moveTo, removeConnection, searchNode, setCurrentFunction, setRuntimeValue, tabIcon, tryCompiling, updateAll, withCurrentFunction_, withCurrentGeometries, withCurrentNode_)
import Lunarbox.Data.Graph as G
import Lunarbox.Data.Route (Route(..))
import Lunarbox.Data.Set (toNative) as Set
import Lunarbox.Foreign.Render (centerNode, centerOutput, setUnconnectableInputs, setUnconnectableOutputs)
import Lunarbox.Foreign.Render as Native
import Record as Record
import Web.Event.Event (preventDefault, stopPropagation)
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
  | Navigate Route
  | LoadScene
  | Rerender
  | DeleteNode NodeId
  | UpdatePreview FunctionName
  | UpdatePreviews
  | MoveTo Location
  -- Handle foreign actions bubbled by the Scene component
  | CreateConnection NodeId NodeId Int
  | SelectInput NodeId Int
  | SelectOutput NodeId
  | HandleConnectionConfirmation PendingConnectionAction
  | GotoId NodeId
  | StartNodeEditing NodeId
  -- This handles actions triggerd by the node editing modal
  | HandleNodeEdits NodeEditingAction

data Output
  = Save Json

type ChildSlots m
  = Add.ChildSlots
      ( tree :: Slot TreeC.Query TreeC.Output Unit
      , scene :: Slot Scene.Query Scene.Output Unit
      , pendingConnection :: Modal.Slot Action PendingConnectionAction m Unit
      , editNode :: Modal.Slot Action NodeEditingAction m Unit
      )

-- Shorthand for manually passing the types of the actions and child slots
-- We use this to automatically focus on the correct element when pressing S
searchNodeInputRef :: RefLabel
searchNodeInputRef = RefLabel "search node"

-- We need this to only trigger events when in focus
searchNodeClassName :: String
searchNodeClassName = "node-search__input"

-- | Describes how we should color the problems icon tab
data ProblemsIconVariant
  = NoProblems
  | Warnings
  | Errors

-- | Actions which can be triggered from the connection confirmation modal.
data PendingConnectionAction
  = AutofixConnection
  | CancelConnecting
  | ProceedConnecting

-- | The actual config for how to display the connection confirmation modal
pendingConnectionModal :: forall m. Modal.InputType PendingConnectionAction Action m
pendingConnectionModal =
  { id: "confirm-connection"
  , title: "Confirm connection"
  , content:
    const
      $ HH.text
          "Connecting those nodes would change the type of this function creating a type error."
  , buttons:
    [ { text: "Continue"
      , primary: true
      , value: ProceedConnecting
      }
    , { text: "Cancel"
      , primary: false
      , value: CancelConnecting
      }
    ]
  , onClose: CancelConnecting
  }

-- | Actions the user can perform in the node editing modal
data NodeEditingAction
  = NEDeleteNode
  | NECloseModal

-- | The config of the modal used for editing nodes
nodeEditingModal :: forall m. Modal.InputType NodeEditingAction Action m
nodeEditingModal =
  { id: "edit-node"
  , title: "[Node name here]"
  , content: \_ -> HH.text "[Content goes here]"
  , buttons:
    [ { text: "Delete"
      , primary: false
      , value: NEDeleteNode
      }
    , { text: "Back"
      , primary: true
      , value: NECloseModal
      }
    ]
  , onClose: NECloseModal
  }

component :: forall m q. MonadAff m => MonadEffect m => MonadReader Config m => Navigate m => Component HH.HTML q State Output m
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
  handleAction :: Action -> HalogenM State Action (ChildSlots m) Output m Unit
  handleAction = case _ of
    Init -> do
      window <- liftEffect Web.window
      document <- liftEffect $ Web.document window
      -- Generate geometries for the current function
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
          unless (currentTab == Add && panelIsOpen) do
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

          -- We take an argument because we don't need to compute this
          -- if the first condition is true
          wouldCycle _ =
            maybe false not $ G.wouldCreateLongCycle <$> bestMatch <*> Just currentFunction
              <*> Just functionGraph
        when
          (Just currentFunction == bestMatch || wouldCycle unit)
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
      handleAction $ UpdatePreview name
      handleAction LoadScene
    SelectFunction name ->
      withCurrentFunction_ \currentFunction ->
        gets (join <<< preview (_function name))
          >>= traverse_ case _ of
              VisualFunction _ ->
                unless (name == currentFunction) do
                  modify_ $ setCurrentFunction name
                  handleAction LoadScene
              _ -> pure unit
    StartFunctionCreation -> do
      void $ query (SProxy :: _ "tree") unit $ tell TreeC.StartCreation
    RemoveConnection id index -> do
      modify_ $ removeConnection $ Tuple id index
      void updateAll
      printString $ "Removed " <> show id <> " w index " <> show index
      handleAction Rerender
    SetRuntimeValue functionName nodeId runtimeValue -> do
      modify_ $ setRuntimeValue functionName nodeId runtimeValue
    ChangeInputCount function amount -> do
      modify_ $ set (_atInputCount function) $ Just amount
      handleAction $ UpdatePreview function
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
        newState <- gets stateToJson
        raise $ Save newState
        handleAction $ Autosave newState
      else
        handleAction $ Autosave oldState
    Navigate route -> navigate route
    Rerender -> do
      -- TODO: optimize this to not rerender the previews on each render
      handleAction UpdatePreviews
      void $ query (SProxy :: SProxy "scene") unit $ tell $ Scene.Rerender
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
        compilationResult = tryCompiling state
      case compilationResult.typeErrors of
        [] -> do
          put $ evaluate $ Record.merge compilationResult state
          void updateAll
        _ -> do
          modify_
            _
              { pendingConnection =
                Just
                  { from
                  , toId
                  , toIndex
                  , compilationResult
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
        >>= traverse_ \{ from, toId, toIndex, compilationResult } -> case other of
            ProceedConnecting -> do
              state <- get
              put
                $ createConnection from toId toIndex
                $ Record.merge compilationResult
                $ state
                    { pendingConnection = Nothing
                    }
              void updateAll
              handleAction Rerender
            AutofixConnection -> modify_ _ { pendingConnection = Nothing }
            -- WARNING: this should never happen
            _ -> pure unit
    DeleteNode id ->
      void
        $ withCurrentGeometries \cache -> do
            current <- gets $ view _currentFunction
            modify_ $ deleteNode current id
            liftEffect $ Native.deleteNode cache id
            void updateAll
            handleAction Rerender
    UpdatePreview name -> do
      state <- get
      for_ (Map.lookup (AtFunction name) state.typeMap) \ty -> do
        let
          inputs =
            fromMaybe' (\_ -> getMaxInputs name state) $ join
              $ preview (_atInputCount name) state
        void $ query (SProxy :: SProxy "nodePreview") name
          $ tell
          $ NodePreview.Rerender
          $ getFunctionColorMap inputs ty
    UpdatePreviews -> do
      functions <- gets $ view _functions
      for_ (Map.keys functions) $ handleAction <<< UpdatePreview
    MoveTo location -> do
      let
        steps = moveTo location
      for_ steps case _ of
        MoveToFunction name -> handleAction $ SelectFunction name
        CenterNode id -> void $ withCurrentGeometries \cache -> liftEffect $ centerNode cache id
        CenterOutput -> void $ withCurrentGeometries \cache -> liftEffect $ centerOutput cache
      -- TODO: don't do this when we didn't move the camera
      unless (Array.null steps) $ handleAction Rerender
    StartNodeEditing id ->
      withCurrentFunction_ \currentFunction -> do
        state <- get
        gets (preview (_atNode currentFunction id))
          >>= traverse_ case _ of
              ComplexNode { function } -> do
                modify_ _ { currentlyEditedNode = Just id }
                mkQuery $ tell $ Modal.UpdateInput modal
                mkQuery $ tell Modal.Open
                where
                mkQuery = void <<< query (SProxy :: SProxy "editNode") unit

                functionData = preview (_atFunctionData function <<< _Just <<< _Newtype) state

                -- | TODO: have the function data provide an actual description of the entire node.
                description = _.output.description <$> functionData

                type' = getNodeType id function state

                inputs =
                  fromMaybe []
                    $ map (uncurry mkInput)
                    <$> inputStuff
                  where
                  inputTypes = Array.fromFoldable <$> Type.inputs <$> type'

                  inputDocs = _.inputs <$> functionData

                  inputStuff = Array.zip <$> inputTypes <*> inputDocs

                  mkInput type'' docs =
                    { type': type''
                    , name: docs.name
                    , description: docs.description
                    }

                modal =
                  nodeEditingModal
                    { title = show function
                    , content =
                      \_ ->
                        EditNode.component
                          { description
                          , type'
                          , inputs
                          }
                    }
              _ -> pure unit
    HandleNodeEdits action -> case action of
      NEDeleteNode -> withCurrentNode_ (handleAction <<< DeleteNode)
      NECloseModal -> pure unit
    GotoId id ->
      withCurrentFunction_ \currentFunction ->
        gets (preview (_atNode currentFunction id))
          >>= traverse_ case _ of
              ComplexNode { function } -> handleAction (SelectFunction function)
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
    Native.EditNode id -> Just $ StartNodeEditing id
    Native.Goto id -> Just $ GotoId id
    _ -> Nothing

  sidebarIcon extraClasses activeTab current =
    HH.div
      [ classes $ ClassName <$> [ "editor__activity" ] <> (guard isActive $> "editor__activity--active") <> extraClasses
      , onClick $ const $ Just $ ChangeTab current
      ]
      [ icon $ tabIcon current ]
    where
    isActive = current == activeTab

  tabs variant currentTab =
    [ icon Settings
    , icon Add
    , icon Tree
    , icon Problems
    ]
    where
    problemClasses = case variant of
      Errors -> [ "editor__activity--error" ]
      Warnings -> [ "editor__activity--warning" ]
      NoProblems -> []

    classes Problems = problemClasses

    classes _ = []

    icon tab = sidebarIcon (classes tab) currentTab tab

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

  isInternal functions functionName = maybe true (is _NativeFunction) $ Map.lookup functionName functions

  panel :: State -> Array (HH.ComponentHTML Action (ChildSlots m) m)
  panel { currentTab
  , project: project@(Project.Project { functions })
  , currentFunction
  , functionData
  , typeMap
  , inputCountMap
  , name
  , isExample
  , isVisible
  , isAdmin
  , nodeSearchTerm
  , typeErrors
  , lintingErrors
  } = case currentTab of
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
              , updatePreview: Just <<< UpdatePreview
              }
          ]
        }
    Problems ->
      mkPanel
        { title: "Problems"
        , actions: []
        , footer: Nothing
        , header: Nothing
        , content:
          [ problems
              { typeErrors
              , lintingErrors
              , navigateTo: MoveTo
              , isInternal: isInternal functions
              }
          ]
        }

  scene :: HH.ComponentHTML Action (ChildSlots m) m
  scene = HH.slot (SProxy :: _ "scene") unit Scene.component unit handleSceneOutput

  logoElement =
    HH.img
      [ HP.src "https://cdn.discordapp.com/attachments/672889285438865453/708081533151477890/favicon.png"
      , HP.alt "Lunarbox logo"
      , className "editor__logo"
      , onMouseUp $ const $ Just $ Navigate Home
      ]

  render :: State -> HH.ComponentHTML Action (ChildSlots m) m
  render state@{ currentTab
  , panelIsOpen
  , typeErrors
  , lintingErrors
  , project: Project.Project ({ functions })
  } =
    HH.div [ className "editor" ]
      [ HH.div [ className "editor__activity-bar" ]
          $ tabs problemsIconVariant currentTab
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
      -- Modals
      , HH.slot (SProxy :: SProxy "pendingConnection") unit Modal.component
          pendingConnectionModal
          handleConnectionConfirmation
      , HH.slot (SProxy :: SProxy "editNode") unit Modal.component
          nodeEditingModal
          handleNodeSave
      ]
    where
    shouldRender' = shouldRender (isInternal functions)

    problemsIconVariant
      | Array.any (shouldRender' <<< TypeError.getLocation) typeErrors = Errors
      | Array.any (shouldRender' <<< LintError.getLocation) lintingErrors = Warnings
      | otherwise = NoProblems

    handleConnectionConfirmation = case _ of
      Modal.ClosedWith value -> Just $ HandleConnectionConfirmation value
      Modal.BubbledAction action -> Just action

    handleNodeSave = case _ of
      Modal.ClosedWith value -> Just $ HandleNodeEdits value
      Modal.BubbledAction action -> Just action
