module Lunarbox.Capability.Editor.Node.Arc
  ( Arc(..)
  , solveOverlaps
  , emptySpaces
  , length
  , fillWith
  , normalize
  , rotate
  , full
  ) where

import Prelude
import Control.MonadZero (guard)
import Data.Foldable (minimumBy)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (ceil, toNumber)
import Data.List (List(..), catMaybes, nub, (..), zip, (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst)
import Debug.Trace (trace)
import Lunarbox.Data.Duplet (Duplet(..))
import Lunarbox.Data.Functor (indexed)
import Lunarbox.Data.List (chunk)
import Lunarbox.Data.Math (normalizeAngle)
import Math (Radians, tau)

-- Data structure representing an arc on a circle
-- The arc also holds an inner value of any type
data Arc a
  = Arc Radians Radians a

-- Typeclass instances
derive instance eqArc :: Eq a => Eq (Arc a)

derive instance functorArc :: Functor Arc

derive instance genericArc :: Generic (Arc a) _

instance showArc :: Show a => Show (Arc a) where
  show = genericShow

-- Get the length of an arc in radians
length :: forall a. Arc a -> Radians
length (Arc start end _) = let delta = end - start in if end > start then delta else tau + delta

-- Normalize angles bigger than 2 pi
normalize :: forall a. Arc a -> Arc a
normalize (Arc start end inner) = Arc (normalizeAngle start) (normalizeAngle end) inner

-- Rotate an arc by a number of radians
rotate :: forall a. Radians -> Arc a -> Arc a
rotate amount (Arc start end inner) = normalize $ Arc (start + amount) (end + amount) inner

-- Credit: https://stackoverflow.com/a/11776964/11012369
intersect :: Number -> Number -> Number -> Boolean
intersect b as ae = (as > ae && (b >= as || b <= ae)) || (b >= as && b <= ae)

-- Check if 2 arcs overlap
intersect' :: forall a. Arc a -> Arc a -> Boolean
intersect' (Arc s e _) (Arc s' e' _) =
  intersect s' s e
    || intersect e' s e
    || intersect s s' e'
    || intersect e s' e'

-- Construct a full circle containing an arbitrary value
full :: forall a. a -> Arc a
full = Arc 0.0 (tau - 0.00001)

-- Get all overlaps between some arcs
collectIntersections :: forall a. Eq a => List (Arc a) -> List (Arc a)
collectIntersections arcs =
  nub do
    a <- arcs
    a' <- arcs
    guard $ a /= a' && intersect' a a'
    a : a' : Nil

moveIntersections :: forall a. Ord a => List (Arc a) -> Duplet (List (Arc a))
moveIntersections arcs = case collectIntersections arcs of
  arc : xs -> case moveIntersections xs of
    Duplet o os -> Duplet (arc : o) os
  Nil -> Duplet Nil arcs

solveOverlaps :: forall a. Ord a => List (Arc a) -> List (List (Arc a))
solveOverlaps arcs = case moveIntersections arcs of
  Duplet Nil a -> pure a
  Duplet a as -> (solveOverlaps a) <> pure as

-- Find the closest arc to the end of a given arc
closestArcStart :: forall a. Ord a => List (Arc a) -> Arc a -> Maybe (Arc a)
closestArcStart arcs target@(Arc targetStart _ _) = fst <$> minimumBy (\(Tuple _ delta) (Tuple _ delta') -> compare delta delta') deltas
  where
  withoutTarget = List.delete target arcs

  deltas =
    ( \arc@(Arc start _ _) ->
        Tuple arc
          $ let
              delta = start - targetStart
            in
              if start > targetStart then delta else tau + delta
    )
      <$> withoutTarget

-- Given a list of arcs returns the empty space on the circle
-- This function assumes the arcs do not overlap
emptySpaces :: forall a. Ord a => List (Arc a) -> List (Arc Unit)
emptySpaces Nil = pure $ full unit

emptySpaces ((Arc start end _) : Nil) = pure $ Arc end start unit

emptySpaces arcs =
  catMaybes
    $ ( \arc@(Arc _ end _) ->
          (\(Arc start _ _) -> Arc end start unit) <$> closestArcStart arcs arc
      )
    <$> arcs

-- Given a list of arcs get the empty spaces and fill them with arcs generated from another list of arcs
fillWith :: forall a. Ord a => List a -> List (Arc a) -> List (Arc a)
fillWith arcs toFill = filled <> toFill
  where
  spaces = normalize <$> emptySpaces toFill

  a = if List.length spaces == 1 then trace spaces identity else unit

  chunkSize = ceil $ (toNumber $ List.length arcs) / (toNumber $ List.length spaces)

  filled =
    (zip spaces $ chunk chunkSize arcs)
      >>= ( \(Tuple arc@(Arc spaceStart _ _) keys) ->
            let
              arcLength = length arc / (toNumber $ List.length keys)
            in
              indexed keys
                <#> \(Tuple index key) ->
                    let
                      start = spaceStart + toNumber index * arcLength
                    in
                      Arc start (start + arcLength) key
        )
