module Bril.Optimizations.DCE.Trivial (runOnProgram) where

import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Optimizations.DCE.Trivial.Global qualified as Global
import Bril.Optimizations.DCE.Trivial.Local qualified as Local
import Bril.Program (Program (..))
import Bril.Program qualified as Program
import Control.Lens ((%~))

-- | @runOnFunction func@ performs both local and global DCE
-- on `func`, iterated to convergence
runOnFunction :: Func -> Func
runOnFunction func =
  if Func.size func' < Func.size func
    then -- Some dead instructions were deleted
      runOnFunction func'
    else func'
  where
    func' = Local.runOnFunction $ Global.runOnFunction func

-- | @runOnProgram prog@ performs both local and global DCE
-- on every function in `prog`, iterated to convergence
runOnProgram :: Program -> Program
runOnProgram = Program.functions %~ map runOnFunction
