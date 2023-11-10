module Bril.Dataflow (Dir (..), Params (..), analyze) where

import Bril.CFG
import Data.Foldable (foldl')
import Data.List qualified as List
import Data.Map (Map, (!))
import Data.Map qualified as Map

-- | The direction of a dataflow analysis
data Dir = Forward | Backward

-- | The parameters needed to characterize a dataflow analysis
data Params facts g = Params
  { -- | The direction of the analysis
    dir :: Dir,
    -- | The top value of the semilattice of dataflow facts
    top :: g -> facts,
    -- | The meet operation in the semilattice of dataflow facts
    meet :: facts -> facts -> facts,
    -- | The transfer function, computing output dataflow facts
    -- given the facts input to a node and the node itself
    transfer :: facts -> NodeOf g -> facts
  }

-- | Run a dataflow analysis, associating every node in the CFG with corresponding dataflow facts.
-- That is, @analyze g node@ are the dataflow facts associated with node `node` in CFG `g`.
analyze :: forall facts g. (IsCFG g, Ord (NodeOf g), Eq facts) => Params facts g -> g -> Map (NodeOf g) (facts, facts)
analyze Params {dir, top, meet, transfer} g = go initialFacts (nodes g)
  where
    -- Initially, are nodes are associated with the top element of the lattice
    initialFacts = Map.fromList $ map (,(initial, initial)) $ nodes g
    -- @go facts w@ iteratively computes the dataflow facts associated with every node of `g`
    -- where `w` is a worklist containing the CFG nodes whose equations may not be satisfied
    go facts [] = facts
    go facts (node : w) = go facts' w'
      where
        lookupFacts = snd . (facts !)
        -- The input to the transfer function
        inputFacts = foldl' meet initial $ map lookupFacts $ dependencies node g
        -- The output of the transfer function
        outputFacts = transfer inputFacts node
        -- The updated dataflow facts after processing this node
        facts' = Map.insert node (inputFacts, outputFacts) facts
        -- The updated worklist after processing this node
        w'
          | outputFacts == lookupFacts node =
              -- dataflow facts computed at this node did not change
              w
          | otherwise =
              -- dataflow facts computed at this node did change
              -- Add the dependents of this node to the worklist
              w `List.union` dependents node g
    (dependencies, dependents) =
      case dir of
        Forward -> (predecessors, successors)
        Backward -> (successors, predecessors)
    initial = top g
