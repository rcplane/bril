import Bril.CFG (IsCFG, IsNode, NodeOf)
import Bril.CFG qualified as CFG
import Bril.CFG.NodeMap qualified as NodeMap
import Bril.Dominator (Relations (..))
import Bril.Dominator qualified as Dom
import Bril.Func (Func)
import Bril.Func qualified as Func
import Bril.Parse
import Bril.Program (Program (..))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Traversable (forM)
import System.FilePath.Glob
import System.Process
import Test.Hspec

-- | @dominators cfg@ is an association list mapping every node to the
-- set of nodes that dominate it, using a naive algorithm. Used as a
-- reference implementation for hspec
dominators :: (IsCFG g, Ord (NodeOf g)) => g -> Map (NodeOf g) (Set (NodeOf g))
dominators cfg = Map.fromList $ map (\b -> (b, dominatorsForNode b)) allNodes
  where
    allNodes = CFG.nodes cfg
    dominatorsForNode b = Set.fromList $ filter (`dom` b) allNodes
    dom a b =
      case CFG.start cfg of
        Nothing -> True
        Just start ->
          if b == start
            then a == start
            else not $ b `Set.member` CFG.reachableExcluding cfg (Set.singleton a) start

-- | Construct a dominator tree naively as a reference implementation
dominatorTree :: (IsCFG g, IsNode (NodeOf g), Ord (NodeOf g)) => g -> Dom.Tree (NodeOf g)
dominatorTree g =
  case CFG.start g of
    Nothing -> Dom.Empty
    Just start -> Dom.Root $ build start
  where
    Dom.Relations {idom} = Dom.relations g
    build node =
      Dom.Node
        { node,
          children =
            g
              & CFG.nodes
              & filter (node `idom`)
              & map build
              & Set.fromList
        }

-- | Compute the dominance frontier
dominanceFrontier :: (IsNode (NodeOf g), IsCFG g, Ord (NodeOf g)) => g -> Map (NodeOf g) (Set (NodeOf g))
dominanceFrontier g =
  g
    & CFG.nodes
    & map (\a -> (a, dominanceFrontierOfNode a))
    & Map.fromList
  where
    allNodes = CFG.nodes g
    Relations {sdom, dom} = Dom.relations g
    dominanceFrontierOfNode a =
      Set.fromList $
        filter (\b -> not (a `sdom` b) && any (a `dom`) (CFG.predecessors b g)) allNodes

-- | @verifyDominators cfg@ returns `True` iff the dataflow implementation
-- of dominators agrees with the naive, slow implementation of dominators
verifyDominators :: (Ord (NodeOf g), IsNode (NodeOf g), IsCFG g) => g -> Bool
verifyDominators g = Dom.dominators g == dominators g

verifyDominatorsForFunction :: Func -> Bool
verifyDominatorsForFunction func =
  func
    & Func.instrs
    & NodeMap.fromList
    & verifyDominators

verifyDominatorsForProgram :: Program -> Bool
verifyDominatorsForProgram (Program funcs) = all verifyDominatorsForFunction funcs

-- | @verifyDominatorTree cfg@ returns `True` iff the reference and test
-- implementations of dominator trees agree
verifyDominatorTree :: (Ord (NodeOf g), IsNode (NodeOf g), IsCFG g) => g -> Bool
verifyDominatorTree g = dominatorTree g == Dom.tree g

verifyDominatorTreeForFunction :: Func -> Bool
verifyDominatorTreeForFunction func =
  func
    & Func.instrs
    & NodeMap.fromList
    & verifyDominatorTree

verifyDominatorTreesForProgram :: Program -> Bool
verifyDominatorTreesForProgram (Program funcs) = all verifyDominatorTreeForFunction funcs

-- | @verifyDominanceFrontier cfg@ returns `True` iff the reference and test
-- implementations of the dominance frontier agree
verifyDominanceFrontier :: (Ord (NodeOf g), IsNode (NodeOf g), IsCFG g) => g -> Bool
verifyDominanceFrontier g = dominanceFrontier g == Dom.frontier (Dom.tree g) g

verifyDominanceFrontierForFunction :: Func -> Bool
verifyDominanceFrontierForFunction func =
  func
    & Func.instrs
    & NodeMap.fromList
    & verifyDominanceFrontier

verifyDominanceFrontierForProgram :: Program -> Bool
verifyDominanceFrontierForProgram (Program funcs) = all verifyDominanceFrontierForFunction funcs

dominatorTests :: [(String, Program)] -> SpecWith ()
dominatorTests progs =
  describe "Test Dominators" do
    forM_ progs \(path, prog) -> do
      it ("dominators for " ++ path) do
        verifyDominatorsForProgram prog `shouldBe` True

dominatorTreeTests :: [(String, Program)] -> SpecWith ()
dominatorTreeTests progs =
  describe "Test Dominator Trees" do
    forM_ progs \(path, prog) -> do
      it ("dominator trees for " ++ path) do
        verifyDominatorTreesForProgram prog `shouldBe` True

dominanceFrontierTests :: [(String, Program)] -> SpecWith ()
dominanceFrontierTests progs =
  describe "Test Dominance Frontier" do
    forM_ progs \(path, prog) -> do
      it ("dominance frontier for " ++ path) do
        verifyDominanceFrontierForProgram prog `shouldBe` True

-- | Associate every benchmark file path with its parsed Bril program
parsePrograms :: IO [(String, Program)]
parsePrograms = do
  paths <- glob "../benchmarks/*/*.bril"
  forM paths \path -> do
    bril <- readProcess "cat" [path] []
    json <- readProcess "bril2json" [] bril
    prog <- decodeProgram $ fromString json
    pure (path, prog)

main :: IO ()
main = do
  progs <- parsePrograms
  hspec $ describe "dominance utilities" do
    dominatorTests progs
    dominatorTreeTests progs
    dominanceFrontierTests progs
