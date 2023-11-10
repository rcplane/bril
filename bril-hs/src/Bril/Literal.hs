module Bril.Literal (Literal (..), parseForType, parseForMaybeType) where

import Bril.Type (Type)
import Bril.Type qualified as Type
import Control.Applicative ((<|>))
import Data.Aeson hiding (Bool)
import Data.Aeson.Types (Parser)
import Data.Int (Int64)

-- | A literal value in bril
data Literal
  = Int Int64
  | Float Double
  | Bool Bool
  deriving (Show)

parseForMaybeType :: Maybe Type -> Value -> Parser Literal
parseForMaybeType (Just ty) j = parseForType ty j
parseForMaybeType Nothing j =
  Int <$> parseJSON j
    <|> Bool <$> parseJSON j
    <|> Float <$> parseJSON j

-- | Parse a literal, given its expected type
parseForType :: Type -> Value -> Parser Literal
parseForType Type.Int j = Int <$> parseJSON j
parseForType Type.Bool j = Bool <$> parseJSON j
parseForType Type.Float j = Float <$> parseJSON j
parseForType (Type.Ptr _) _ = error "Cannot parse pointer literal"

instance ToJSON Literal where
  toJSON (Int i) = toJSON i
  toJSON (Float f) = toJSON f
  toJSON (Bool b) = toJSON b