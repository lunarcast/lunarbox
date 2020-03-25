module Lunarbox.Data.Graph where

import Prelude
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Graph as CG
import Data.Lens (Traversal', lens, traversed, wander)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, uncurry)
import Data.Unfoldable (class Unfoldable)

newtype Graph k v
  = Graph (Map.Map k (Tuple v (Set.Set k)))

derive instance newtypeGraph :: Newtype (Graph k v) _

derive instance eqGraph :: (Eq k, Eq v) => Eq (Graph k v)

derive instance ordGraph :: (Ord k, Ord v) => Ord (Graph k v)

instance functorGraph :: Ord k => Functor (Graph k) where
  map f (Graph m) = Graph (map (lmap f) m)

instance semigroupGraph :: (Ord k, Semigroup v) => Semigroup (Graph k v) where
  append (Graph m) (Graph m') = Graph $ Map.unionWith f m m'
    where
    f (Tuple v k) (Tuple v' k') = Tuple (v <> v') $ k `Set.union` k'

instance monoidGraph :: (Ord k, Semigroup v) => Monoid (Graph k v) where
  mempty = Graph $ Map.empty

instance foldableGraph :: Ord k => Foldable (Graph k) where
  foldMap f = foldMap (f <<< fst) <<< unwrap
  foldr = foldrDefault
  foldl = foldlDefault

instance traversableGraph :: Ord k => Traversable (Graph k) where
  traverse f (Graph m) = Graph <$> traverse (uncurry f') m
    where
    f' v k = (flip Tuple $ k) <$> f v
  sequence = sequenceDefault

instance indexGraph :: Ord k => Index (Graph k v) k v where
  -- no idea what this does... 
  -- copied from the source of the profunctor-lesnes library
  -- modified just a tiny bit to work with graphs
  ix k =
    wander \coalg m ->
      lookup k m
        # maybe
            (pure m)
            (coalg >>> map \v -> insert k v m)

instance atGraph :: Ord k => At (Graph k v) k v where
  -- good thing at least I understand this one:)
  at k = lens (lookup k) \m -> maybe (delete k m) (\v -> insert k v m)

singleton :: forall k v. Ord k => k -> v -> Graph k v
singleton k v = Graph $ Map.singleton k $ Tuple v Set.empty

insert :: forall k v. Ord k => k -> v -> Graph k v -> Graph k v
insert k v (Graph m) = Graph $ Map.insert k (Tuple v Set.empty) m

lookup :: forall k v. Ord k => k -> Graph k v -> Maybe v
lookup k = map fst <<< Map.lookup k <<< unwrap

delete :: forall k v. Ord k => k -> Graph k v -> Graph k v
delete k = over Graph $ Map.delete k

keys :: forall k v. Ord k => Graph k v -> Set k
keys = Map.keys <<< unwrap

vertices :: forall k v. Ord k => Graph k v -> List v
vertices = map fst <<< Map.values <<< unwrap

toUnfoldable :: forall u k v. Unfoldable u => Ord k => Graph k v -> u (Tuple k v)
toUnfoldable (Graph m) = Map.toUnfoldable $ fst <$> m

-- no idea how to implement this so I'm using an implementation from another lib
topologicalSort :: forall k v. Ord k => Graph k v -> List k
topologicalSort = CG.topologicalSort <<< CG.fromMap <<< unwrap

_Graph :: forall k v. Ord k => Traversal' (Graph k v) v
_Graph = traversed
