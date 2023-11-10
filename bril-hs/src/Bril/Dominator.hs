module Bril.Dominator
  ( dominators,
    relations,
    Relations (..),
    Tree (..),
    Node (..),
    tree,
    frontier,
  )
where

import Bril.CFG (IsCFG (..), IsNode (..))
import Bril.Dataflow (Dir (..), Params (..))
import Bril.Dataflow qualified as Dataflow
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Effectful (runPureEff)
import Effectful.State.Static.Local

transfer :: (Ord node, IsNode node) => Set node -> node -> Set node
transfer facts node
  | isStart node = Set.singleton node
  | otherwise = Set.insert node facts

-- | The Facts computed by the dominator analysis; sets of nodes
type Facts g = Set (NodeOf g)

params :: forall g. (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => Params (Facts g) g
params =
  Params
    { dir = Forward,
      top = Set.fromList . nodes,
      meet = Set.intersection,
      transfer
    }

-- | @dominators cfg node@ is the set of all nodes in `cfg` that dominate `node`
dominators :: (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => g -> Map (NodeOf g) (Facts g)
dominators = Map.map snd . Dataflow.analyze params

-- | Various dominance relations
data Relations node = Relations
  { dom :: node -> node -> Bool,
    sdom :: node -> node -> Bool,
    idom :: node -> node -> Bool
  }

-- | The [strict | immediate] dominance relations for a CFG
relations :: (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => g -> Relations (NodeOf g)
relations g =
  Relations {dom, sdom, idom}
  where
    dom a b = a `Set.member` doms b
    sdom a b = a /= b && a `dom` b
    idom a b = a `sdom` b && all (\c -> b == c || not (a `sdom` c)) (doms b)
    doms = (dominators g !)

-- | A dominance tree, where A is the direct parent of B iff A immediately dominates B
data Tree node
  = Root (Node node)
  | Empty
  deriving (Eq)

-- | A non-empty dominance tree
data Node node = Node
  { node :: node,
    children :: Set (Node node)
  }
  deriving (Eq)

instance (Ord node) => Ord (Node node) where
  compare (Node u _) (Node v _) = compare u v

-- | @tree cfg@ is the dominance tree associated with `cfg`
tree :: (IsCFG g, Ord (NodeOf g), IsNode (NodeOf g)) => g -> Tree (NodeOf g)
tree g =
  case start g of
    Nothing -> Empty
    Just root -> Root $ buildFromRoot root
  where
    doms = (dominators g !)
    strictDominators b = Set.delete b $ doms b
    buildFromRoot node =
      Node
        { node,
          children =
            strictlyDominated
              & Set.toList
              & filter (\child -> strictlyDominated `Set.disjoint` strictDominators child)
              & map buildFromRoot
              & Set.fromList
        }
      where
        allNodes = nodes g
        dominatedBy = (foldl' updateForNode initialMap allNodes !)
          where
            updateForNode m dst =
              dst
                & doms -- All nodes `src` such that `src dom dst`
                & foldl' (flip $ Map.adjust $ Set.insert dst) m
            initialMap =
              allNodes
                & map (,Set.empty)
                & Map.fromList
        strictlyDominatedBy src = Set.delete src $ dominatedBy src
        strictlyDominated = strictlyDominatedBy node

-- | @frontier node cfg@ are the nodes in `cfg` on the dominance frontier of `node`
frontier :: (Ord (NodeOf g), IsCFG g) => Tree (NodeOf g) -> g -> Map (NodeOf g) (Set (NodeOf g))
frontier root g =
  case root of
    Empty -> Map.empty
    Root node -> runPureEff $ execState Map.empty $ go node
  where
    go Node {node, children} = do
      (childDoms, childFrontiers) <- unzip <$> traverse go (Set.toList children)
      let doms = Set.unions childDoms
          succs = Set.fromList $ successors node g
          rootFrontier =
            (succs `Set.union` Set.unions childFrontiers) \\ doms
      modify (Map.insert node rootFrontier)
      pure (Set.insert node doms, rootFrontier)
