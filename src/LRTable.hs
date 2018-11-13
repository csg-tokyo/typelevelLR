
module LRTable where

import Syntax
import LALRAutomaton

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.List     (nub)
import Control.Monad (mzero, forM_)

-------------------------------------------------------------------------------

newtype LRTable = LRTable { getLRTable :: Map.Map LRNode (Map.Map Terminal LRAction) }
  deriving (Show)

data LRAction = Shift  LRNode
              | Reduce Rule
              | Accept
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

lrTable :: LRAutomaton -> LRTable
lrTable automaton = LRTable . Map.fromList $ do
  node <- lrAutomatonNodes automaton
  let accepts = if acceptible node then Map.singleton EndOfInput Accept else Map.empty
  let shifts = Map.fromListWith (error "shift-shift conflict") $ do
        t <- nub $ lrNodeItems node >>= \(item, _) -> case lrItemNext item of
          Just (TerminalSymbol t, _) -> return t
          _                          -> mzero
        case lrAutomatonNext automaton node (TerminalSymbol t) of
          Just node' -> return (t, Shift node')
          _          -> error "no such transition found"
  let reduces = Map.fromListWith (error "reduce-reduce conflict") $ do
        (LRItem rule rest, lookAhead) <- lrNodeItems node
        if null rest then [(t, Reduce rule) | t <- Set.toList lookAhead] else mzero
  -- shift priored
  -- Map.union == Map.unionWith const
  return (node, accepts `Map.union` shifts `Map.union` reduces)

-------------------------------------------------------------------------------

printLRTable :: LRTable -> IO ()
printLRTable table = do
  forM_ (Map.toList (getLRTable table)) $ \(src, edges) -> do
    putStrLn (show src ++ ":")
    forM_ (Map.toList edges) $ \(t, dst) -> do
      putStrLn ("  " ++ show t ++ " -> " ++ show dst)

-------------------------------------------------------------------------------

lrTableTransitions :: LRTable -> [(LRNode, Terminal, LRAction)]
lrTableTransitions table = do
  (node, line  ) <- Map.toList (getLRTable table)
  (t   , action) <- Map.toList line
  return (node, t, action)

-------------------------------------------------------------------------------
