module Bril.Fresh
  ( fresh,
    nextAvailable,
    runFreshWithPostfix,
    Fresh (..),
    runFresh,
  )
where

import Data.Foldable qualified as Foldable
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (Eff, Effect)
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.State.Static.Local
import Effectful.TH

-- | An effect supporting the generation of fresh variables
data Fresh :: Effect where
  -- | @fresh x@ is a fresh variable with prefix `x`
  Fresh :: Text -> Fresh m Text

makeEffect ''Fresh

-- | @nextAvailable x unavailable@ is a variable with prefix
-- `x` not in `unavailable`
nextAvailable :: Text -> Set Text -> Maybe Text
nextAvailable x unavailable =
  Foldable.find (\newName -> not $ Set.member newName unavailable) $
    map (\(n :: Integer) -> x <> Text.pack (show n)) [1 ..]

-- | Run the fresh effect with a specified postfix and set of
-- unavailable variables
runFreshWithPostfix :: Text -> Set Text -> Eff (Fresh ': es) a -> Eff es a
runFreshWithPostfix postfix initiallyUnavailable =
  reinterpret (evalState initiallyUnavailable) \_ (Fresh x) -> do
    unavailable <- get
    let next = fromJust $ nextAvailable (x <> postfix) unavailable
    modify (Set.insert next)
    pure next

-- | Run the fresh effect with a specified set of initially
-- unvailable variables
runFresh :: Set Text -> Eff (Fresh ': es) a -> Eff es a
runFresh = runFreshWithPostfix ""
