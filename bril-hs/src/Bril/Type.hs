module Bril.Type (Type (..)) where

import Data.Aeson hiding (Bool)
import Data.Aeson.Types hiding (Bool)

-- | The type of a Bril variable
data Type
  = Int
  | Bool
  | Float
  | Ptr Type
  deriving (Show)

instance FromJSON Type where
  parseJSON (String "int") = pure Int
  parseJSON (String "bool") = pure Bool
  parseJSON (String "float") = pure Float
  parseJSON (Object obj) = Ptr <$> obj .: "ptr"
  parseJSON _ = parseFail "Cannot parse type"

instance ToJSON Type where
  toJSON Int = "int"
  toJSON Float = "float"
  toJSON Bool = "bool"
  toJSON (Ptr ty) = object ["ptr" .= ty]
