module Lunarbox.Component.Editor.Add
  ( add
  , ChildSlots
  ) where

import Prelude
import Data.Default (def)
import Data.Int (fromString, toNumber)
import Data.List ((!!))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Effect.Class (class MonadEffect)
import Halogen (Slot)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties as HP
import Lunarbox.Capability.Editor.Type (prettify)
import Lunarbox.Component.Editor.HighlightedType (highlightTypeToHTML)
import Lunarbox.Component.Editor.NodePreview as NodePreview
import Lunarbox.Component.Icon (icon)
import Lunarbox.Component.Utils (className, whenElem)
import Lunarbox.Data.Dataflow.Type (Type, inputs, output)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Location (Location(..))
import Lunarbox.Data.Editor.Node.NodeDescriptor (NodeDescriptor, describe)
import Lunarbox.Data.Editor.Node.PinLocation (Pin(..))
import Lunarbox.Data.Editor.Project (Project)
import Lunarbox.Data.Editor.State (getMaxInputs)
import Lunarbox.Data.Ord (sortBySearch)

type ChildSlots r
  = ( nodePreview :: Slot NodePreview.Query NodePreview.Output FunctionName | r )

type Input
  = { project :: Project
    , currentFunction :: FunctionName
    , functionData :: Map.Map FunctionName FunctionData
    , typeMap :: Map.Map Location Type
    , inputCountMap :: Map.Map FunctionName Int
    , nodeSearchTerm :: String
    }

type Actions a
  = { edit :: FunctionName -> Maybe a
    , delete :: FunctionName -> Maybe a
    , addNode :: FunctionName -> Maybe a
    , changeInputCount :: FunctionName -> Int -> Maybe a
    , updatePreview :: FunctionName -> Maybe a
    }

resolvePin :: Pin -> Type -> Maybe Type
resolvePin (InputPin index) type' = inputs type' !! index

resolvePin OutputPin type' = Just $ output type'

-- The little icon buttons next to each node
nodeButton :: forall a s m. Boolean -> Maybe a -> String -> HH.ComponentHTML a s m
nodeButton active handleClick iconName =
  whenElem active \_ ->
    HH.div
      [ onClick $ const handleClick
      ]
      [ icon iconName ]

makeNode ::
  forall r a m.
  MonadEffect m =>
  Actions a ->
  NodeDescriptor ->
  FunctionName ->
  Int ->
  Map.Map Location Type ->
  Map.Map FunctionName Int -> FunctionData -> HH.ComponentHTML a (ChildSlots r) m
makeNode { edit, addNode, changeInputCount, delete, updatePreview } { isUsable, isEditable, canBeDeleted } name maxInputs typeMap inputCountMap functionData =
  HH.div [ className "node" ]
    [ HH.slot (SProxy :: SProxy "nodePreview") name NodePreview.component { name } $ const $ updatePreview name
    , HH.div [ className "node__data" ]
        [ HH.div [ className "node__text" ]
            [ HH.header [ className "node__data-header" ]
                [ HH.div [ className "node__name no-overflow" ]
                    [ HH.text $ show name
                    ]
                , nodeButton isUsable (addNode name) "add"
                , nodeButton isEditable (edit name) "edit"
                , nodeButton canBeDeleted (delete name) "delete"
                ]
            , HH.section [ className "node__type" ]
                $ fromMaybe mempty
                $ pure
                <<< highlightTypeToHTML
                <<< prettify
                <$> Map.lookup (AtFunction name) typeMap
            , HH.section [ className "node__currying" ]
                [ HH.div [ className "node__curying-text" ] [ HH.text "inputs:" ]
                , HH.input
                    [ HP.value $ show inputCount
                    , HP.type_ $ HP.InputNumber
                    , HP.min 0.0
                    , HP.max $ toNumber maxInputs
                    , className "node__currying-input"
                    , onValueInput $ changeInputCount name <=< map (clamp 0 maxInputs) <<< fromString
                    ]
                ]
            ]
        ]
    ]
  where
  inputCount = fromMaybe maxInputs $ Map.lookup name inputCountMap

add :: forall r a m. MonadEffect m => Input -> Actions a -> HH.ComponentHTML a (ChildSlots r) m
add input@{ project, currentFunction, functionData, typeMap, inputCountMap, nodeSearchTerm } actions =
  HH.div [ className "nodes" ]
    $ ( \(Tuple name descriptor) ->
          let
            functionData' = fromMaybe def $ Map.lookup name functionData
          in
            makeNode actions descriptor name (getMaxInputs name input) typeMap inputCountMap functionData'
      )
    <$> sortBySearch (show <<< fst) nodeSearchTerm
        ( Map.toUnfoldable
            $ describe currentFunction project
        )
