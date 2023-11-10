module Bril.Func
  ( BasicBlock (..),
    Func (..),
    name,
    args,
    ty,
    blocks,
    instrs,
    formBasicBlocks,
    Arg (..),
    uses,
    vars,
    argNames,
    defs,
    size,
    defsOf,
  )
where

import Bril.BasicBlock (BasicBlock (..))
import Bril.BasicBlock qualified as BB
import Bril.Expr (Var)
import Bril.Fresh (fresh, runFresh)
import Bril.Instr (Instr, Instr' (..), Label, isTerminator, _Label)
import Bril.Instr qualified as Instr
import Bril.Type (Type (..))
import Control.Lens (makeLenses, preview, view, views)
import Control.Monad (forM)
import Data.Aeson
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Effectful (runPureEff)

-- | Split an instruction stream into basic blocks
splitAtTerminators :: [Instr] -> [[Instr]]
splitAtTerminators = filter (not . null) . go []
  where
    go curr [] = [reverse curr]
    go curr (lbl@(Label _) : is) = reverse curr : go [lbl] is
    go curr (instr : is)
      | isTerminator instr = reverse (instr : curr) : go [] is
      | otherwise = go (instr : curr) is

-- | Form a sequence of basic blocks from a sequence of instructions
formBasicBlocks :: [Instr] -> [BasicBlock]
formBasicBlocks instrs =
  (BB.start :) $
    runPureEff $
      runFresh existingLabels $
        forM grouped \case
          is@(Label l : _) -> pure $ BasicBlock l Map.empty is
          is -> do
            freshLabel <- fresh "lbl."
            pure $ BasicBlock freshLabel Map.empty (Label freshLabel : is)
  where
    grouped = splitAtTerminators instrs
    existingLabels = Set.fromList $ mapMaybe (preview _Label) instrs

-- | An argument to a function
data Arg = Arg
  { name :: Var,
    ty :: Type
  }
  deriving (Show)

instance ToJSON Arg where
  toJSON (Arg name ty) =
    object ["name" .= name, "type" .= ty]

instance FromJSON Arg where
  parseJSON = withObject "arg" \obj ->
    Arg <$> obj .: "name" <*> obj .: "type"

-- | A Bril function
data Func = Func
  { _name :: Text,
    _args :: [Arg],
    _ty :: Maybe Type,
    _blocks :: [BasicBlock]
  }
  deriving (Show)

makeLenses ''Func

-- | The instructions in a Bril function
instrs :: Func -> [Instr]
instrs = concatMap (view BB.instrs) . view blocks

-- | The number of instructions in a Bril function
size :: Func -> Int
size = length . instrs

-- | All variables used by some instruction in a Bril function
uses :: Func -> Set Var
uses = Set.fromList . concatMap Instr.uses . instrs

-- | All variables defined in a function
defs :: Func -> Set Var
defs func = Set.fromList $ argNames func ++ mapMaybe Instr.def (instrs func)

argNames :: Func -> [Var]
argNames = views args $ map \Arg {name = argName} -> argName

defsOf :: Func -> Map Var (Set Label)
defsOf func@Func {_blocks} = Map.unionsWith Set.union (paramDefs : bbDefs)
  where
    paramDefs =
      func
        & argNames
        & map (,Set.singleton $ view BB.name $ head _blocks)
        & Map.fromList
    bbDefs =
      _blocks
        & map \bb ->
          bb
            & BB.defs
            & Set.toList
            & map (,Set.singleton $ view BB.name bb)
            & Map.fromList

-- | All variables referenced in a function
vars :: Func -> Set Var
vars func = Set.union (uses func) (defs func)

instance FromJSON Func where
  parseJSON =
    withObject "function" \obj ->
      Func
        <$> obj .: "name"
        <*> obj .:? "args" .!= []
        <*> obj .:? "type"
        <*> (formBasicBlocks <$> obj .: "instrs")

instance ToJSON Func where
  toJSON Func {_name, _args, _ty, _blocks} =
    object $
      maybeToList (("type" .=) <$> _ty)
        ++ [ "name" .= _name,
             "args" .= _args,
             "instrs" .= concatMap blockToInstrs _blocks
           ]
    where
      blockToInstrs bb =
        case insts of
          lbl@(Label _) : rest -> toJSON lbl : phiJSONs ++ map toJSON rest
          _ -> phiJSONs ++ map toJSON insts
        where
          phiJSONs = map toJSON (Map.elems $ view BB.phiNodes bb)
          insts = view BB.instrs bb
