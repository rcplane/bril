module Main (main) where

import Bril.Optimizations.DCE qualified as DCE
import Bril.Optimizations.DCE.Trivial qualified as TDCE
import Bril.Optimizations.LVN qualified as LVN
import Bril.Parse (decodeProgram)
import Bril.Program (Program)
import Bril.SSA qualified as SSA
import Control.Monad (guard)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (foldl')
import Data.Function
import Data.Functor
import Data.Maybe (catMaybes)
import Options.Applicative

data Options = Options
  { tdce :: Bool,
    dce :: Bool,
    lvn :: Bool,
    ssa :: Bool
  }

options :: Parser Options
options =
  Options
    <$> switch (long "tdce" <> help "Perform trivial dead code elimination")
    <*> switch (long "dce" <> help "Perform dead code elimination")
    <*> switch (long "lvn" <> help "Perform local value numbering")
    <*> switch (long "ssa" <> help "Perform static single assignment rewriting")

type Optimization = Program -> Program

requestedOptimizations :: Options -> [Optimization]
requestedOptimizations Options {tdce, dce, lvn, ssa} =
  catMaybes
    [ guard ssa $> SSA.runOnProgram,
      guard lvn $> LVN.runOnProgram,
      guard dce $> DCE.runOnProgram,
      guard tdce $> TDCE.runOnProgram
    ]

parseOptions :: IO Options
parseOptions = execParser parserInfo
  where
    parserInfo =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Optimize Bril programs")

runOptimizations :: [Optimization] -> Program -> Program
runOptimizations opts prog = foldl' (&) prog opts

main :: IO ()
main = do
  contents <- LBS.getContents
  prog <- decodeProgram contents
  opts <- parseOptions
  let optimizations = requestedOptimizations opts
  LBS.putStr $ encodePretty $ runOptimizations optimizations prog
