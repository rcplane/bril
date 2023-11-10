module Bril.SSA (runOnProgram) where

import Bril.BasicBlock qualified as BB
import Bril.CFG.NodeMap (CFG)
import Bril.CFG.NodeMap qualified as CFG
import Bril.Dominator qualified as Dom
import Bril.Expr (Expr' (..), Var)
import Bril.Fresh (Fresh, fresh, runFreshWithPostfix)
import Bril.Func (BasicBlock, Func)
import Bril.Func qualified as Func
import Bril.Instr (Instr, Instr' (..), Label, setDef)
import Bril.Instr qualified as Instr
import Bril.Literal
import Bril.Phi qualified as Phi
import Bril.Program (Program)
import Bril.Program qualified as Program
import Control.Lens (view, views, (%~), (&), (.~), (^.))
import Control.Monad (foldM, forM, forM_)
import Data.Foldable (foldl')
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.State.Static.Local (State, execState, get, gets, modify, runState)

insertPhis :: Dom.Tree (CFG.Node Label BasicBlock) -> Func -> CFG Label BasicBlock
insertPhis tree func =
  foldl'
    (\cfg x -> go cfg x Set.empty $ Set.toList $ defs ! x)
    (CFG.fromFunc func)
    vars
  where
    defs = Func.defsOf func
    vars = Map.keys defs
    initialCFG = CFG.fromFunc func
    frontierMap = Dom.frontier @(CFG Label BasicBlock) tree initialCFG
    go cfg _ _ [] = cfg
    go cfg x processed (d : ds)
      | d `Set.member` processed = go cfg x processed ds
      | otherwise =
          go cfg' x (Set.insert d processed) (Set.toList frontier ++ ds)
      where
        cfg' = foldl' (flip (CFG.insertPhi x)) cfg frontier
        frontier :: Set Label
        frontier = Set.map (view BB.name . view CFG.value) $ frontierMap ! CFG.findNode d initialCFG

renameInstrs :: (State (Map Var Var) :> es, Fresh :> es) => [Instr] -> Eff es [Instr]
renameInstrs = mapM \instr -> do
  renames <- get
  let renamedInstr = fmap (renames !) instr
  case Instr.def renamedInstr of
    Nothing -> pure renamedInstr
    Just dest -> do
      newDest <- fresh dest
      modify (Map.insert dest newDest)
      pure $ setDef newDest renamedInstr

renamePhiDest :: (State (Map Var Var) :> es, Fresh :> es) => BasicBlock -> Eff es (Map Var Phi.Node)
renamePhiDest bb =
  Map.fromList <$> forM (views BB.phiNodes Map.toList bb) \(dest, phi) -> do
    newDest <- fresh dest
    modify (Map.insert dest newDest)
    pure (newDest, phi & Phi.dest .~ newDest)

rename :: (Fresh :> es) => Map Var Var -> CFG Label BasicBlock -> Dom.Node (CFG.Node Label BasicBlock) -> Eff es (CFG Label BasicBlock)
rename renames cfg Dom.Node {node, children} = do
  (block, renames') <- runState renames renameBlock
  let withRewrittenBlock = CFG.setValue label block cfg
      succs = cfgNode ^. CFG.succs
      withRewrittenSuccs = foldl' (\acc dst -> CFG.modifyValue dst (BB.renamePhiUses renames' label) acc) withRewrittenBlock succs
  foldM (rename renames') withRewrittenSuccs children
  where
    label = node ^. CFG.index
    cfgNode = CFG.findNode label cfg
    renameBlock :: (State (Map Var Var) :> es, Fresh :> es) => Eff es BasicBlock
    renameBlock = do
      let block = cfgNode ^. CFG.value
      phiNodes <- renamePhiDest block
      instrs <- renameInstrs (block ^. BB.instrs)
      pure (block & (BB.phiNodes .~ phiNodes) . (BB.instrs .~ instrs))

-- | Convert a function from SSA, removing all Phi nodes
fromSSA :: CFG Label BasicBlock -> CFG Label BasicBlock
fromSSA cfg =
  runPureEff $
    execState cfg do
      labels <- gets CFG.keys
      forM_ labels \label -> do
        node <- gets (CFG.findNode label)
        let block = node ^. CFG.value
            predecessors = Set.toList $ node ^. CFG.preds
        forM_ (block ^. BB.phiNodes) \phi -> do
          let undefs = Map.fromList $ map (,Const (Int 0)) predecessors
              phiAssigns = fmap Id (phi ^. Phi.args)
              assigns = Map.union phiAssigns undefs
              dest = phi ^. Phi.dest
          forM_ (Map.toList assigns) \(lbl, e) -> do
            let assign = Assign dest Nothing e
            modify $ CFG.modifyValue lbl $ BB.addInstr assign
        modify $ CFG.modifyValue label BB.deletePhis

-- | Convert a function to SSA
toSSA :: Func -> CFG Label BasicBlock
toSSA func =
  renameCFG $ insertPhis tree func
  where
    tree = Dom.tree initialCFG
    initialCFG = CFG.fromFunc func
    paramRenames = Map.fromList $ map (\arg -> (arg, arg)) $ Func.argNames func
    renameCFG cfg =
      case tree of
        Dom.Empty -> cfg
        Dom.Root node ->
          runPureEff $
            runFreshWithPostfix "." (Func.defs func) $
              rename paramRenames cfg node

roundtrip :: Func -> Func
roundtrip func =
  func & Func.blocks .~ CFG.values (fromSSA $ toSSA func)

runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map roundtrip
