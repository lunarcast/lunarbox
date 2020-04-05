module Lunarbox.Capability.Editor.Node.NodeInput (Arc(..), solveOverlaps) where

import Prelude
import Control.MonadZero (guard)
import Data.List (List(..), group, sortBy, (:))
import Data.List.NonEmpty (head, length)
import Data.Tuple (Tuple(..), snd)

data Arc a
  = Arc Number Number a

derive instance eqArc :: Eq a => Eq (Arc a)

derive instance functorArc :: Functor Arc

instance showArc :: Show a => Show (Arc a) where
  show (Arc s e v) = "Arc(" <> show v <> ", [" <> show s <> ", " <> show e <> "])"

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

-- Get all overlaps between some arcs
colleceIntersections :: forall a. Eq a => List (Arc a) -> List (Arc (Tuple Int a))
colleceIntersections arcs =
  (\arcs' -> let arc = head arcs' in (Tuple $ length arcs') <$> arc)
    <$> group do
        a <- arcs
        a' <- arcs
        guard $ a /= a' && intersect' a a'
        a : a' : Nil

getSortedIntersections :: forall a. Ord a => List (Arc a) -> List (Arc a)
getSortedIntersections = (map snd <$> _) <<< sortBy (\(Arc _ _ t) (Arc _ _ t') -> compare t t') <<< colleceIntersections

moveIntersections :: forall a. Ord a => List (Arc a) -> List (List (Arc a))
moveIntersections arcs = case getSortedIntersections arcs of
  arc : xs -> case moveIntersections xs of
    o : os -> (arc : o) : os
    Nil -> pure $ pure arc
  Nil -> pure arcs

solveOverlaps :: forall a. Ord a => List (Arc a) -> List (List (Arc a))
solveOverlaps arcs = case moveIntersections arcs of
  a : Nil -> a : Nil
  a : as -> (solveOverlaps a) <> as
  Nil -> Nil
