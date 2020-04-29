module Lunarbox.Data.Editor.Save
  ( PermanentState
  , stateToJson
  , jsonToState
  ) where

import Prelude
import Data.Argonaut (Json, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Map (Map)
import Data.Tuple (Tuple)
import Lunarbox.Data.Dataflow.Native.Prelude (loadPrelude)
import Lunarbox.Data.Editor.Camera (Camera)
import Lunarbox.Data.Editor.FunctionData (FunctionData)
import Lunarbox.Data.Editor.FunctionName (FunctionName)
import Lunarbox.Data.Editor.Node.NodeData (NodeData)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Editor.Project (Project)
import Lunarbox.Data.Editor.State (State, compile, emptyState)
import Record as Record

type PermanentState r
  = ( project :: Project
    , nextId :: Int
    , nodeData :: Map (Tuple FunctionName NodeId) NodeData
    , functionData :: Map FunctionName FunctionData
    , cameras :: Map FunctionName Camera
    | r
    )

-- Encoding and decoding
stateToJson :: forall r. { | PermanentState r } -> Json
stateToJson { project, nextId, nodeData, functionData, cameras } = encodeJson { project, nextId, nodeData, functionData, cameras }

jsonToState :: forall a s m. Json -> Either String (State a s m)
jsonToState json = do
  obj :: { | PermanentState () } <- decodeJson json
  let
    baseState = Record.merge obj emptyState
  pure $ compile $ loadPrelude baseState
