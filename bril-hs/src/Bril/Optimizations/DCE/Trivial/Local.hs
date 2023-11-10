module Bril.Optimizations.DCE.Trivial.Local (runOnFunction) where

import Bril.BasicBlock qualified as BB
import Bril.Func (BasicBlock (..), Func (..))
import Bril.Func qualified as Func
import Bril.Instr (Instr)
import Bril.Instr qualified as Instr
import Control.Lens ((%~))
import Data.Text (Text)

-- | @defReachesUse x instrs@ is @True@ if there exists some
-- instruction in @instrs@ that defined `x` before it is used
defReachesUse :: Text -> [Instr] -> Bool
defReachesUse _ [] = True
defReachesUse def (i : is)
  | def `elem` Instr.uses i = True
  | Instr.def i == Just def =
      -- `x` is redefined before a use
      False
  | otherwise = defReachesUse def is

-- | @runOnBasicBlock bb@ is `bb` with all pure instructions whose defs
-- never reach a use removed, iterated to convergence
runOnBasicBlock :: BasicBlock -> BasicBlock
runOnBasicBlock = BB.instrs %~ iterateToConvergence
  where
    iterateToConvergence is =
      let is' = removeDeadInstrs is
       in if length is' < length is
            then -- If instruction stream smaller, deleted dead instruction
              iterateToConvergence is'
            else is'
    removeDeadInstrs [] = []
    removeDeadInstrs (i : is) =
      case Instr.def i of
        Just def ->
          if not (Instr.isPure i) || defReachesUse def is
            then -- If `i` has side effects or its def reaches a use, it is live
              i : removeDeadInstrs is
            else removeDeadInstrs is
        Nothing -> i : removeDeadInstrs is

runOnFunction :: Func -> Func
runOnFunction = Func.blocks %~ map runOnBasicBlock
