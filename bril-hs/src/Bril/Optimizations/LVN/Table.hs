module Bril.Optimizations.LVN.Table
  ( Table,
    Value,
    canonicalHome,
    setVNForVar,
    lookupValue,
    insertValue,
    empty,
    valueFromExpr,
    exprFromValue,
  )
where

import Bril.Expr (Expr, Expr' (..))
import Bril.Expr qualified as Expr
import Bril.Literal (Literal)
import Control.Lens (makeLenses, set, view, (%~), (+~), (^.))
import Control.Monad (guard)
import Data.Functor (void)
import Data.List (find)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import Effectful
import Effectful.State.Static.Local

type Value = Expr' Integer

data Entry = Entry
  { _number :: Integer,
    _value :: Maybe Value,
    _var :: Text
  }

makeLenses ''Entry

data Table = Table
  { _entries :: [Entry],
    _var2num :: [(Text, Integer)],
    _nextNum :: Integer
  }

makeLenses ''Table

freshVN :: (State Table :> es) => Eff es Integer
freshVN = do
  vn <- gets (view nextNum)
  modify (nextNum +~ 1)
  pure vn

lookupEntryForVN :: Integer -> Table -> Entry
lookupEntryForVN num =
  fromJust
    . find (\entry -> num == entry ^. number)
    . view entries

setVNForVar :: (State Table :> es) => Text -> Integer -> Eff es ()
setVNForVar x vn = do
  tbl <- get
  let maybePrevEntry = lookupEntryForVar x tbl
  modify $ var2num %~ insertAssoc x vn
  case maybePrevEntry of
    Just prevEntry -> do
      v2n <- gets (view var2num)
      let prevVN = view number prevEntry
      case lookup prevVN (map swap v2n) of
        Just newHome -> modify $ entries %~ (set var newHome prevEntry :)
        Nothing -> pure ()
    Nothing -> pure ()

insertMaybeValue :: (State Table :> es) => Text -> Maybe Value -> Eff es Entry
insertMaybeValue x v = do
  vn <- freshVN
  let entry = Entry vn v x
  modify $ entries %~ (entry :)
  setVNForVar x vn
  pure entry

lookupEntryForVar :: Text -> Table -> Maybe Entry
lookupEntryForVar x tbl = do
  num <- lookup x (view var2num tbl)
  pure $ lookupEntryForVN num tbl

lookupOrCreateEntryForVar :: (State Table :> es) => Text -> Eff es Entry
lookupOrCreateEntryForVar x = do
  tbl <- get
  case lookupEntryForVar x tbl of
    Just entry -> pure entry
    Nothing -> insertMaybeValue x Nothing

canonicalHome :: (State Table :> es) => Text -> Eff es Text
canonicalHome x = view var <$> lookupOrCreateEntryForVar x

lookupVN :: (State Table :> es) => Text -> Eff es Integer
lookupVN x = view number <$> lookupOrCreateEntryForVar x

lookupConst :: Integer -> Table -> Maybe Literal
lookupConst vn tbl = do
  Const lit <- view value (lookupEntryForVN vn tbl)
  pure lit

constFold :: Table -> Value -> Maybe Literal
constFold tbl e = Expr.constFold =<< traverse (`lookupConst` tbl) e

valueFromExpr :: (State Table :> es) => Expr -> Eff es Value
valueFromExpr e = do
  e' <- traverse lookupVN e
  tbl <- get
  pure $ maybe e' Const (constFold tbl e')

exprFromValue :: Value -> Table -> Expr
exprFromValue v tbl =
  case v of
    Const lit -> Const lit
    _ -> fmap (\vn -> view var $ lookupEntryForVN vn tbl) v

lookupValue :: Value -> Table -> Maybe (Integer, Expr)
lookupValue (Id x) tbl = pure (x, expr)
  where
    Entry {_var, _value} = lookupEntryForVN x tbl
    expr = case _value of
      Just (Const lit) -> Const lit
      _ -> Id _var
lookupValue e Table {_entries} =
  listToMaybe $
    flip mapMaybe _entries \Entry {_number, _value, _var} -> do
      val <- _value
      guard $ e == val
      pure (_number, Id _var)

removeAssoc :: (Eq a) => a -> [(a, b)] -> [(a, b)]
removeAssoc k = filter $ (/= k) . fst

insertAssoc :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
insertAssoc k v m = (k, v) : removeAssoc k m

insertValue :: (State Table :> es) => Text -> Value -> Eff es ()
insertValue x = void . insertMaybeValue x . Just

empty :: Table
empty = Table [] [] 1
