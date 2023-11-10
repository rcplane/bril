module Bril.Program (Program (..), functions) where

import Bril.Func (Func)
import Control.Lens (makeLenses)
import Data.Aeson

-- | A Bril program; that is, a series of functions
newtype Program = Program
  { _functions :: [Func]
  }
  deriving (Show)

makeLenses ''Program

instance FromJSON Program where
  parseJSON = withObject "program" \obj ->
    Program <$> obj .: "functions"

instance ToJSON Program where
  toJSON (Program funcs) = object ["functions" .= funcs]
