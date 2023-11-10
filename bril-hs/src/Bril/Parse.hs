module Bril.Parse (decodeProgram) where

import Bril.Program (Program)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import System.Exit
import System.IO

decodeProgram :: ByteString -> IO Program
decodeProgram contents = do
  case eitherDecode contents of
    Left err -> do
      hPutStrLn stderr err
      exitWith (ExitFailure 1)
    Right prog -> pure prog