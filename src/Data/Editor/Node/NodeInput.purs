module Lunarbox.Data.Editor.Node.NodeInput
  ( getArcs
  ) where

import Prelude
import Data.Default (def)
import Data.Int (toNumber)
import Data.Lens (view)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Typelevel.Num (d0, d1)
import Data.Vec ((!!))
import Debug.Trace (trace)
import Lunarbox.Capability.Editor.Node.Arc (Arc(..), fillWith, normalize, rotate, solveOverlaps)
import Lunarbox.Data.Editor.Node (Node, _nodeInputs)
import Lunarbox.Data.Editor.Node.NodeData (NodeData, _NodeDataPosition)
import Lunarbox.Data.Editor.Node.NodeId (NodeId)
import Lunarbox.Data.Functor (indexed)
import Lunarbox.Data.List (alterLast)
import Math (Radians, atan2, pi)

-- 90 degrees in radians
halfPi :: Radians
halfPi = pi / 2.0

-- Get a list of input arcs for any node
getArcs :: (Map.Map NodeId NodeData) -> NodeData -> Node -> List (List (Arc Int))
getArcs nodeDataMap nodeData node = rotated
  where
  inputs = indexed $ view _nodeInputs node

  connected = List.catMaybes $ (uncurry $ (<$>) <<< Tuple) <$> inputs

  maxHalfArcLength =
    pi
      / ( let
            count = toNumber $ List.length connected
          in
            if count == 1.0 then
              2.0
            else
              count
        )

  position = view _NodeDataPosition nodeData

  connectedArcs =
    solveOverlaps
      $ ( \(Tuple index nodeId) ->
            let
              targetPosition = (view _NodeDataPosition $ fromMaybe def $ Map.lookup nodeId nodeDataMap) - position

              x = -(targetPosition !! d0)

              y = targetPosition !! d1

              angle = atan2 x y
            in
              normalize $ Arc (angle - maxHalfArcLength) (angle + maxHalfArcLength) index
        )
      <$> connected

  a = if connectedArcs == pure Nil <> Nil then unit else trace connectedArcs identity

  unconnected = fst <$> List.filter (isNothing <<< snd) inputs

  allArcs =
    if List.null unconnected then
      connectedArcs
    else
      alterLast Nil (fillWith unconnected) connectedArcs

  rotated = (map $ rotate halfPi) <$> allArcs
