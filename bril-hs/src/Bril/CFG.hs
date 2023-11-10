module Bril.CFG
  ( IsNode (..),
    ControlFlow (..),
    IsCFG (..),
    DynCFG (..),
    reachable,
    reachableExcluding,
    pruneUnreachable,
  )
where

import Data.Foldable (foldl')
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Text (Text)

-- | Describes a CFG node
class IsNode a where
  -- | Whether this node is the start node in the CFG
  isStart :: a -> Bool

-- | Represents a node in the CFG that transfers control to another node
class ControlFlow node where
  -- | The label of a cfg node
  label :: node -> Maybe Text

  -- | Whether control flow of a node may fall through to the next
  fallsThrough :: node -> Bool

  -- | All the labels a node references
  labels :: node -> [Text]

-- | Described the operations that can be performed on an immutable control flow graph
class IsCFG a where
  -- | The type of nodes in the CFG
  type NodeOf a

  -- | A list of all nodes in the CFG
  nodes :: a -> [NodeOf a]

  -- | @succs node g@ is a list of all the successors of `node` in `g`
  successors :: NodeOf a -> a -> [NodeOf a]

  -- | @preds node g@ is a list of all the predecessors of `node` in `g`
  predecessors :: NodeOf a -> a -> [NodeOf a]

  -- | @start g@ is the start node of `g`, if any
  start :: a -> Maybe (NodeOf a)

-- | A "dynamic" CFG, where nodes and edges can be inserted/deleted
class (IsCFG g) => DynCFG g where
  -- | @deleteNode u g@ is `g` with node `u` and all of its adjacent edges deleted
  deleteNode :: NodeOf g -> g -> g

visit :: (Ord (NodeOf g), IsCFG g) => g -> Set (NodeOf g) -> NodeOf g -> Set (NodeOf g)
visit g visited node
  | node `Set.member` visited = visited
  | otherwise = foldl' (visit g) (Set.insert node visited) $ successors node g

-- | @reachable start cfg@ are the nodes in `cfg` reachable from `start`
reachable :: (Ord (NodeOf g), IsCFG g) => NodeOf g -> g -> Set (NodeOf g)
reachable src g = visit g Set.empty src

reachableExcluding :: (Ord (NodeOf g), IsCFG g) => g -> Set (NodeOf g) -> NodeOf g -> Set (NodeOf g)
reachableExcluding g excluded node = visit g excluded node \\ excluded

pruneUnreachable :: (DynCFG g, Ord (NodeOf g)) => g -> g
pruneUnreachable g =
  case start g of
    Nothing -> g
    Just root ->
      let unreachable = Set.fromList (nodes g) \\ reachable root g
       in foldl' (flip deleteNode) g unreachable
