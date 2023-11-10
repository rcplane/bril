{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Bril.CFG.NodeMap
  ( Node,
    CFG,
    index,
    value,
    preds,
    succs,
    fromForest,
    fromList,
    insertPhi,
    fromFunc,
    findNode,
    setValue,
    modifyValue,
    keys,
    values,
  )
where

import Bril.BasicBlock (BasicBlock)
import Bril.BasicBlock qualified as BB
import Bril.CFG (ControlFlow, DynCFG (..), IsCFG, IsNode (..), pruneUnreachable)
import Bril.CFG qualified as CFG
import Bril.Expr (Var)
import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Instr (Label)
import Bril.Phi qualified as Phi
import Control.Lens (makeLenses, view, (%~), (.~), (^.))
import Data.Foldable (foldl')
import Data.Function (on, (&))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | A node containing exactly one Bril instruction
data Node k v = Node
  { -- | The zero-based index of this instruction in the instruction stream
    _index :: k,
    -- | The instruction wrapped by this node
    _value :: v,
    -- | The indices of the predecessors of this node
    _preds :: Set k,
    -- | The indices of the successors of this node
    _succs :: Set k
  }
  deriving (Show)

makeLenses ''Node

createNode :: k -> v -> Node k v
createNode k v = Node k v Set.empty Set.empty

instance (Eq k) => Eq (Node k v) where
  (==) = (==) `on` view index

instance (Ord k) => Ord (Node k v) where
  compare = compare `on` view index

-- | A mapping from every index in the instruction stream to a CFG
-- node containing the instruction at that index
type NodeMap k v = Map k (Node k v)

-- | A control flow graph where every node contains exactly one instruction
data CFG k v = CFG
  { _order :: [k],
    _nodes :: NodeMap k v
  }
  deriving (Show)

makeLenses ''CFG

-- | Insert an edge into the CFG
insertEdge :: (Ord k) => Node k v -> Node k v -> NodeMap k v -> NodeMap k v
insertEdge src dst =
  Map.adjust (succs %~ Set.insert dstKey) srcKey
    . Map.adjust (preds %~ Set.insert srcKey) dstKey
  where
    srcKey = src ^. index
    dstKey = dst ^. index

instance IsNode (Node Int v) where
  isStart Node {_index} = _index == 0

instance IsNode (Node Label v) where
  isStart Node {_index} = _index == "__start"

findNode :: (Ord k) => k -> CFG k v -> Node k v
findNode u (CFG _ g) = g Map.! u

instance (Ord k) => IsCFG (CFG k v) where
  type NodeOf (CFG k v) = Node k v
  nodes CFG {_order, _nodes} = map (_nodes Map.!) _order
  successors Node {_succs} (CFG _ g) = map (g Map.!) $ Set.toList _succs
  predecessors Node {_preds} (CFG _ g) = map (g Map.!) $ Set.toList _preds

  start CFG {_order, _nodes} = (_nodes Map.!) <$> listToMaybe _order

instance (Ord k) => DynCFG (CFG k v) where
  deleteNode Node {_index, _preds, _succs} CFG {_order, _nodes} =
    CFG
      { _order = List.delete _index _order,
        _nodes =
          _nodes
            & deleteIncoming
            & deleteOutgoing
            & Map.delete _index
      }
    where
      deleteIncoming g =
        Set.foldl' (flip (Map.adjust (succs %~ Set.delete _index))) g _preds
      deleteOutgoing g =
        Set.foldl' (flip (Map.adjust (preds %~ Set.delete _index))) g _succs

instance (ControlFlow v) => ControlFlow (Node k v) where
  label = CFG.label . view value
  fallsThrough = CFG.fallsThrough . view value
  labels = CFG.labels . view value

-- | Look up the index of a label in the instruction stream
nodeWithLabel :: (ControlFlow v) => [Node k v] -> Label -> Node k v
nodeWithLabel ns = (Map.fromList labels Map.!)
  where
    labels = mapMaybe (\node -> (,node) <$> CFG.label node) ns

forest :: (Ord k) => [Node k v] -> NodeMap k v
forest =
  Map.fromList . map \node -> (node ^. index, node)

-- | Construct a CFG from an instruction stream
fromForest :: (Ord k, ControlFlow v) => [Node k v] -> CFG k v
fromForest instrs =
  pruneUnreachable $
    CFG
      { _order = map (view index) instrs,
        _nodes = addEdgesForInstr instrs (forest instrs)
      }
  where
    lookupLabel = nodeWithLabel instrs
    addEdgesForInstr [] cfg = cfg
    addEdgesForInstr (src : insts@(dst : _)) cfg
      | CFG.fallsThrough src =
          -- If control flow falls through from this instruction to the next, add an edge
          cfg
            & withEdgesForLabels src
            & insertEdge src dst
            & addEdgesForInstr insts
      | otherwise =
          cfg
            & withEdgesForLabels src
            & addEdgesForInstr insts
    addEdgesForInstr [src] cfg = withEdgesForLabels src cfg
    withEdgesForLabels src cfg =
      foldl' (\g' l -> insertEdge src (lookupLabel l) g') cfg succLabels
      where
        succLabels = CFG.labels src

modifyValue :: (Ord k) => k -> (v -> v) -> CFG k v -> CFG k v
modifyValue k f =
  nodes %~ Map.adjust (value %~ f) k

setValue :: (Ord k) => k -> v -> CFG k v -> CFG k v
setValue k v =
  nodes %~ Map.adjust (value .~ v) k

insertPhi :: (Ord k) => Var -> k -> CFG k BasicBlock -> CFG k BasicBlock
insertPhi x u g = modifyValue u (BB.insertPhi phi) g
  where
    phi = Phi.create x predecessorLabels
    predecessorLabels =
      map (view (value . BB.name)) (CFG.predecessors (findNode u g) g)

-- | @forest instrs@ is a CFG with a node for every instruction in @instrs@ but no edges
fromList :: (ControlFlow v) => [v] -> CFG Int v
fromList instrs = fromForest $ zipWith createNode [0 ..] instrs

fromFunc :: Func -> CFG Label BasicBlock
fromFunc func =
  fromForest $ flip map (func ^. Func.blocks) \block ->
    createNode (block ^. BB.name) block

values :: (Ord k) => CFG k v -> [v]
values = map (view value) . CFG.nodes

keys :: CFG k v -> [k]
keys = view order

mapValues :: (Ord k) => (a -> a) -> CFG k a -> CFG k a
mapValues f cfg =
  foldl' (\acc k -> modifyValue k f acc) cfg $ keys cfg
