module Bril.Phi
  ( Edge (..),
    Node (..),
    create,
    label,
    var,
    dest,
    args,
    replaceUse,
    assignments,
  )
where

import Bril.Expr (Var)
import Bril.Instr (Label)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as Map

-- | Represents an argument to a phi node
data Edge = Edge
  { -- | The label of the basic block
    _label :: Label,
    -- | The name of the variable
    _var :: Var
  }
  deriving (Show)

makeLenses ''Edge

-- | A phi node
data Node = Node
  { -- | The variable this Phi node writes to
    _dest :: Var,
    -- | The arguments to this Phi node
    _args :: Map Label Var
  }
  deriving (Show)

makeLenses ''Node

instance ToJSON Node where
  toJSON Node {_dest, _args} =
    object
      [ "dest" .= _dest,
        "op" .= ("phi" :: String),
        "labels" .= Map.keys _args,
        "args" .= Map.elems _args
      ]

create :: Var -> [Label] -> Node
create x preds =
  Node
    { _dest = x,
      _args =
        preds
          & map (,x)
          & Map.fromList
    }

replaceUse :: Map Var Var -> Label -> Node -> Node
replaceUse renames l = args %~ Map.update (`Map.lookup` renames) l

assignments :: Node -> [(Var, Label, Var)]
assignments Node {_dest, _args} =
  _args
    & Map.toList
    & map (\(l, x) -> (_dest, l, x))
