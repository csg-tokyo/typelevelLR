
module LRTable where

import Syntax
import LALRAutomaton

-------------------------------------------------------------------------------

newtype LRTable = LRTable { getLRTable :: Map.Map LRNode (Map.Map Terminal LRAction) }

data LRAction = Shift  LRNode
              | Reduce Rule
              | Accept

-------------------------------------------------------------------------------

lrTable :: LRAutomaton -> LRTable
lrTable automaton = LRTable . Map.fromList $ do
  node <- lrAutomatonNodes automaton
  let edges = lrAutomatonEdges automaton node
  let items = lrNodeItems node
  let actions = Map.fromList $ do
        t <- lrAutomatonTerminals automaton
        action <- case filter (\(sym, _) -> sym == TerminalSymbol t) edges of
          [(_, dst)] -> return (Shift dst)
          []         -> case t of
            EndOfInput -> if any (\(item, _) -> item == LRItem startRule []) items then return Accept else mzero
            _          -> case filter (\(item, la) -> Set.member t la && null (lrItemRest item)) items of
              [(item, _)] -> return (Reduce (lrItemRule item))
              []          -> mzero
              _           -> error "reduce reduce conflict"
          _          -> error "shift shift conflict ??"
        return (t, action)
  return (node, actions)

-------------------------------------------------------------------------------
