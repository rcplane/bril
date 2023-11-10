module Bril.Instr
  ( Instr,
    Instr' (..),
    _Label,
    Label,
    uses,
    def,
    setDef,
    destType,
    isTerminator,
    opcode,
    isPure,
  )
where

import Bril.CFG (ControlFlow (..))
import Bril.Expr (Expr, Expr' (..), Var)
import Bril.Expr qualified as Expr
import Bril.Literal (Literal, parseForMaybeType)
import Bril.Type (Type)
import Control.Applicative ((<|>))
import Control.Lens (has, makePrisms, preview)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Data.Text (Text)

-- | The type of a Bril Label
type Label = Text

-- | A Bril instruction, parameterized by the type of uses
data Instr' a
  = Assign Text (Maybe Type) (Expr' a)
  | Label Label
  | Jmp Label
  | Br a Label Label
  | CallEff Text [a]
  | Ret (Maybe a)
  | Print [a]
  | Nop
  | Free a
  | Store a a
  | Speculate
  | Commit
  | Guard a Label
  deriving (Show, Functor, Foldable, Traversable)

makePrisms ''Instr'

-- | An instruction with variable operands
type Instr = Instr' Var

-- | All operands used by an instruction
uses :: Instr' a -> [a]
uses (Assign _ _ e) = Expr.uses e
uses (Label _) = []
uses (Jmp _) = []
uses (Br cond _ _) = [cond]
uses (CallEff _ args) = args
uses (Ret arg) = maybeToList arg
uses (Print args) = args
uses Nop = []
uses (Free ptr) = [ptr]
uses (Store ptr val) = [ptr, val]
uses Speculate = []
uses Commit = []
uses (Guard cond _) = [cond]

instance ControlFlow (Instr' a) where
  label = preview _Label
  fallsThrough instr =
    -- An instruction transfers control to the next instruction
    -- if it is a guard (which falls through if the condition is true)
    -- or the instruction is not a terminator; that is, it does not
    -- terminate a basic block
    has _Guard instr || not (isTerminator instr)

  labels (Jmp l) = [l]
  labels (Br _ t f) = [t, f]
  labels _ = []

-- | The variable defined by an instruction, if any
def :: Instr' a -> Maybe Var
def (Assign x _ _) = pure x
def _ = Nothing

setDef :: Var -> Instr' a -> Instr' a
setDef y (Assign _ t e) = Assign y t e
setDef _ inst = inst

-- | The type of a variable defined by an instruction, if any
destType :: Instr' a -> Maybe Type
destType (Assign _ t _) = t
destType _ = Nothing

-- | All functions referenced by an instruction
funcs :: Instr' a -> [Text]
funcs (CallEff func _) = [func]
funcs (Assign _ _ (Call func _)) = [func]
funcs _ = []

-- | Whether an instruction is a terminator;
-- an instruction that may not transfer control
-- flow to the next instruction in the sequence
isTerminator :: Instr' a -> Bool
isTerminator (Ret _) = True
isTerminator (Jmp _) = True
isTerminator (Br {}) = True
isTerminator (Guard _ _) = True
isTerminator _ = False

parseUnary :: (Text -> Expr) -> Object -> Parser Instr
parseUnary unop obj = do
  (dest, ty) <- parseDest obj
  [x] <- obj .: "args"
  pure $ Assign dest ty $ unop x

parseDest :: Object -> Parser (Text, Maybe Type)
parseDest obj =
  (,) <$> obj .: "dest" <*> obj .:? "type"

parseBinary :: (Text -> Text -> Expr) -> Object -> Parser Instr
parseBinary binop obj = do
  (dest, ty) <- parseDest obj
  [x, y] <- obj .: "args"
  pure $ Assign dest ty $ binop x y

parseJmp :: Object -> Parser Instr
parseJmp obj = do
  [l] <- obj .: "labels"
  pure $ Jmp l

parseBr :: Object -> Parser Instr
parseBr obj = do
  [cond] <- obj .: "args"
  [t, f] <- obj .: "labels"
  pure $ Br cond t f

parseCall :: Object -> Parser Instr
parseCall obj = do
  args <- obj .:? "args" .!= []
  [func] <- obj .: "funcs"
  let parseCallValue = do
        (dest, t) <- parseDest obj
        pure $ Assign dest t $ Call func args
  parseCallValue <|> pure (CallEff func args)

parseRet :: Object -> Parser Instr
parseRet obj = do
  obj .:? "args" >>= \case
    Nothing -> pure $ Ret Nothing
    Just (_ : _ : _) -> parseFail "Too many return values"
    Just args -> pure $ Ret $ listToMaybe args

parsePrint :: Object -> Parser Instr
parsePrint obj = Print <$> obj .:? "args" .!= []

parseFree :: Object -> Parser Instr
parseFree obj = do
  [x] <- obj .: "args"
  pure $ Free x

parseStore :: Object -> Parser Instr
parseStore obj = do
  [ptr, val] <- obj .: "args"
  pure $ Store ptr val

parseGuard :: Object -> Parser Instr
parseGuard obj = do
  [cond] <- obj .: "args"
  [onFail] <- obj .: "labels"
  pure $ Guard cond onFail

parseConst :: Object -> Parser Instr
parseConst obj = do
  (dest, ty) <- parseDest obj
  val <- parseForMaybeType ty =<< obj .: "value"
  pure $ Assign dest ty (Const val)

parseInstr :: Object -> Parser Instr
parseInstr obj =
  obj .: "op" >>= withText "opcode" \case
    "add" -> parseBinary Add obj
    "sub" -> parseBinary Sub obj
    "mul" -> parseBinary Mul obj
    "div" -> parseBinary Div obj
    "fadd" -> parseBinary FAdd obj
    "fsub" -> parseBinary FSub obj
    "fmul" -> parseBinary FMul obj
    "fdiv" -> parseBinary FDiv obj
    "eq" -> parseBinary Eq obj
    "lt" -> parseBinary Lt obj
    "gt" -> parseBinary Gt obj
    "le" -> parseBinary Le obj
    "ge" -> parseBinary Ge obj
    "feq" -> parseBinary FEq obj
    "flt" -> parseBinary FLt obj
    "fgt" -> parseBinary FGt obj
    "fle" -> parseBinary FLe obj
    "fge" -> parseBinary FGe obj
    "not" -> parseUnary Not obj
    "and" -> parseBinary And obj
    "or" -> parseBinary Or obj
    "jmp" -> parseJmp obj
    "br" -> parseBr obj
    "call" -> parseCall obj
    "ret" -> parseRet obj
    "id" -> parseUnary Id obj
    "const" -> parseConst obj
    "print" -> parsePrint obj
    "nop" -> pure Nop
    "free" -> parseFree obj
    "store" -> parseStore obj
    "alloc" -> parseUnary Alloc obj
    "load" -> parseUnary Load obj
    "ptradd" -> parseBinary PtrAdd obj
    "speculate" -> pure Speculate
    "commit" -> pure Commit
    "guard" -> parseGuard obj
    _ -> parseFail "Unknown opcode"

parseLabel :: Object -> Parser Instr
parseLabel obj = Label <$> obj .: "label"

instance FromJSON Instr where
  parseJSON =
    withObject "Instr" \obj ->
      parseInstr obj <|> parseLabel obj

opcode :: Instr' a -> Text
opcode (Assign _ _ e) = Expr.opcode e
opcode (Jmp _) = "jmp"
opcode (Br {}) = "br"
opcode (CallEff _ _) = "call"
opcode (Ret _) = "ret"
opcode (Print _) = "print"
opcode Nop = "nop"
opcode (Free _) = "free"
opcode (Store _ _) = "store"
opcode Speculate = "speculate"
opcode Commit = "commit"
opcode (Guard _ _) = "guard"
opcode (Label _) = error "Labels have no opcode"

value :: Instr' a -> Maybe Literal
value (Assign _ _ (Const lit)) = pure lit
value _ = Nothing

isPure :: Instr' a -> Bool
isPure Nop = True
isPure (Assign _ _ e) = Expr.isPure e
isPure _ = False

instance ToJSON Instr where
  toJSON (Label lbl) = object ["label" .= lbl]
  toJSON instr =
    object $
      optionalFields
        ++ [ "op" .= opcode instr,
             "labels" .= labels instr,
             "funcs" .= funcs instr,
             "args" .= uses instr
           ]
    where
      optionalFields =
        catMaybes
          [ ("dest" .=) <$> def instr,
            ("type" .=) <$> destType instr,
            ("value" .=) <$> value instr
          ]
