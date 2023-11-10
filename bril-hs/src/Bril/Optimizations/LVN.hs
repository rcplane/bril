module Bril.Optimizations.LVN (runOnProgram) where

import Bril.BasicBlock qualified as BB
import Bril.Expr (Var)
import Bril.Func (BasicBlock (..), Func (..))
import Bril.Func qualified as Func
import Bril.Instr (Instr, Instr' (..))
import Bril.Instr qualified as Instr
import Bril.Optimizations.LVN.RenameTable (RenameTable)
import Bril.Optimizations.LVN.RenameTable qualified as RenameTable
import Bril.Optimizations.LVN.Table (Table)
import Bril.Optimizations.LVN.Table qualified as Table
import Bril.Program (Program (..))
import Bril.Program qualified as Program
import Control.Lens ((%~))
import Effectful
import Effectful.State.Static.Local

-- | Renames all variables contained by some structure
renameUses :: (Functor f) => RenameTable -> f Var -> f Var
renameUses renames = fmap (`RenameTable.find` renames)

-- | Whether a variable is defined by some instruction in a sequence
definedBy :: (Foldable t) => Var -> t (Instr' a) -> Bool
definedBy x = any \i -> Instr.def i == Just x

-- | Renames all variables with multiple definitions in a basic block
-- to have names unique within that basic block
renameVariables :: [Instr] -> [Instr]
renameVariables instrs = go (RenameTable.fromBasicBlock instrs) instrs
  where
    go _ [] = []
    go renames (i@(Assign x ty e) : is)
      | definedBy x is =
          -- If `x` is redefined in the remaining instructions, rename it
          let e' = renameUses renames e
              (x', renames') = RenameTable.rename x renames
           in Assign x' ty e' : go renames' is
      | otherwise =
          renameUses renames i : go (RenameTable.delete x renames) is
    go renames (i : is) = renameUses renames i : go renames is

-- | Replaces every variable an instruction with is canonical
-- representative in the value number table
canonicalizeUses :: (State Table :> es) => Instr -> Eff es Instr
canonicalizeUses = traverse Table.canonicalHome

-- | Perform value numbering on a sequence of instructions
-- forming a basic block and stateful updates on the value
-- number table in the process
runOnInstrs :: (State Table :> es) => [Instr] -> Eff es [Instr]
runOnInstrs [] = pure []
runOnInstrs (Assign x ty e : instrs) = do
  value <- Table.valueFromExpr e
  gets (Table.lookupValue value) >>= \case
    Just (num, canonical) -> do
      -- expression already in table
      -- var2num[x] = num
      Table.setVNForVar x num
      (Assign x ty canonical :) <$> runOnInstrs instrs
    Nothing -> do
      -- expression not previously in table
      -- x is now its canonical home
      e' <- gets (Table.exprFromValue value)
      Table.insertValue x value
      (Assign x ty e' :) <$> runOnInstrs instrs
runOnInstrs (instr : instrs) = (:) <$> canonicalizeUses instr <*> runOnInstrs instrs

-- | Perform value numbering on a basic block
runOnBasicBlock :: BasicBlock -> BasicBlock
runOnBasicBlock = BB.instrs %~ runPureEff . evalState Table.empty . runOnInstrs . renameVariables

-- | Perform LVN on every basic block in a function
runOnFunction :: Func -> Func
runOnFunction = Func.blocks %~ map runOnBasicBlock

-- | Perform LVN on every basic block in a program.
-- This includes constant propagation, CSE, copy propagation, and algebraic identites.
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
