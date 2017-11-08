
{-# LANGUAGE FlexibleContexts #-}

module LALRAutomaton (module LALRAutomaton) where

import Syntax
import Utility

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo(appEndo))
import Control.Monad         (forM_, mzero)
import Control.Monad.Writer  (MonadWriter(), execWriter)
import Control.Arrow         ((***))
import Data.Maybe            (fromJust)

-------------------------------------------------------------------------------

data LRItem = LRItem {
  lrItemNonTerminal :: NonTerminal,
  lrItemRuleName    :: String,
  lrItemExpression  :: [Symbol],
  lrItemRest        :: [Symbol]
} deriving (Eq, Ord)

-------------------------------------------------------------------------------

instance Show LRItem where
  showsPrec _ (LRItem nt ruleName expr rest) = appEndo $ execWriter $ do
    tells "LRItem("
    tells ruleName
    tells " : "
    tells (nonTerminalName nt)
    tells " -> "
    let (fs, bs) = splitAt (length expr - length rest) expr
    forM_ fs $ \symbol -> tellSymbol symbol >> tells " "
    tells "."
    forM_ rest $ \symbol -> tells " " >> tellSymbol symbol
    tells ")"

-------------------------------------------------------------------------------

type LRClosure = Map.Map LRItem (Set.Set Terminal)

mergeLRClosure :: LRClosure -> LRClosure -> LRClosure
mergeLRClosure = Map.unionWith Set.union

completeLRClosure :: Syntax -> FirstSetTable -> LRClosure -> LRClosure
completeLRClosure syntax table = fixPoint (mergeLRClosure <$> id <*> grow)
  where grow c = Map.fromListWith Set.union $ do
          (item, la) <- Map.toList c
          case lrItemRest item of
            Left nt : rest' -> do
              (ruleName, expr) <- syntaxRules syntax nt
              return (LRItem nt ruleName expr expr, firstSet syntax table la rest')
            _               -> mzero

initialLRClosure :: Syntax -> FirstSetTable -> LRClosure
initialLRClosure syntax table = completeLRClosure syntax table seed
  where seed = Map.singleton (LRItem StartSymbol "(start)" [Left start] [Left start]) (Set.singleton EndOfInput)
        start = syntaxStart syntax
        rules = syntaxRules syntax

transitions :: Syntax -> FirstSetTable -> LRClosure -> Map.Map Symbol LRClosure
transitions syntax table c = fmap (completeLRClosure syntax table) $
  Map.fromListWith mergeLRClosure $ do
    (LRItem nt ruleName expr rest, la) <- Map.toList c
    case rest of
      []             -> mzero
      symbol : rest' -> return (symbol, Map.singleton (LRItem nt ruleName expr rest') la)

-------------------------------------------------------------------------------

data LR1Automaton = LR1Automaton {
  lr1AutomatonStart  :: LRClosure,
  lr1AutomatonEdges  :: Map.Map LRClosure (Map.Map Symbol LRClosure)
}

lr1Automaton :: Syntax -> LR1Automaton
lr1Automaton syntax = LR1Automaton start edges
  where table = firstSetTable syntax
        start = initialLRClosure syntax table
        edges = fixPoint grow initialEdges
        initialEdges = Map.singleton start (transitions syntax table start)
        grow edges = Map.unionsWith (Map.unionWith mergeLRClosure) $
          (edges :) $ do
            es <- Map.elems edges
            c  <- Map.elems es
            if Map.member c edges then mzero
              else return (Map.singleton c (transitions syntax table c))
            
-------------------------------------------------------------------------------

{-
data LALRAutomaton = LALRAutomaton {
  lalrAutomatonStart :: LRClosure,
  lalrAutomatonEdges :: Map.Map LRClosure (Map.Map Symbol LRClosure)
}

canMerge :: Map.Map LRClosure (Map.Map Symbol LRClosure) -> LRClosure -> LRClosure -> Bool
canMerge edges c1 c2 = and [equaling Map.keysSet c1 c2,
                            equaling referents   c1 c2]
  where referents c = fmap (fmap hideSelfReferencing) (Map.lookup c edges)
        hideSelfReferencing c = if c == c1 || c == c2 then Map.empty else c

compactEdges :: LRClosure -> LRClosure -> Map.Map LRClosure (Map.Map Symbol LRClosure) -> Map.Map LRClosure (Map.Map Symbol LRClosure)
compactEdges c1 c2 edges = edges
  -- 1. remove c1, c2 from edges
  |> Map.delete c1 . Map.delete c2
  -- 2. add c' to edges
  |> Map.insertWith (Map.unionWith mergeLRClosure) c' rhs'
  -- 3. replace c1 and c2 by c' in the right hand side of edges
  |> fmap (fmap (\c -> if c == c1 || c == c2 then c' else c))
  where c' = mergeLRClosure c1 c2
        rhs1 = Map.findWithDefault Map.empty c1 edges
        rhs2 = Map.findWithDefault Map.empty c2 edges
        rhs' = Map.unionWith mergeLRClosure rhs1 rhs2

lalrAutomaton :: Syntax -> LALRAutomaton
lalrAutomaton syntax = LALRAutomaton start edges'
  where LR1Automaton start edges = lr1Automaton syntax
        edges' = fixPoint compact edges
        compact es = foldr (uncurry compactEdges) es (mergeablePairs es)
        mergeablePairs es = [(c1, c2) | (c1, c2) <- allPairs (Map.keys es),
                              canMerge es c1 c2]
-}

type LRNode = Set.Set LRItem

data LALRAutomaton = LALRAutomaton {
  lalrAutomatonStart :: LRNode,
  lalrAutomatonEdges :: Map.Map LRNode (Map.Map Symbol LRNode)
}

lalrAutomaton :: Syntax -> LALRAutomaton
lalrAutomaton syntax = LALRAutomaton (Map.keysSet start) edges'
  where LR1Automaton start edges = lr1Automaton syntax
        edges' = Map.toList edges
                 |> map (Map.keysSet *** fmap Map.keysSet)
                 |> Map.fromListWith (Map.unionWith Set.union)

-------------------------------------------------------------------------------

showEdges :: (MonadWriter (Endo String) m) => LALRAutomaton -> m ()
showEdges (LALRAutomaton start edges) = do
  forM_ (Map.toList edges) $ \(src, dsts) -> do
    if src == start
      then tellsLn (show (Set.toList src) ++ "  <-- start")
      else tellsShowLn (Set.toList src)
    forM_ (Map.toList dsts) $ \(symbol, dst) -> do
      tellsLn (" * " ++ appEndo (execWriter (tellSymbol symbol)) "")
      tellsLn ("   => " ++ show (Set.toList dst))

printEdges :: LALRAutomaton -> IO ()
printEdges automaton = putStr (appEndo (execWriter (showEdges automaton)) "")

-------------------------------------------------------------------------------

-- lalrAutomatonNodes :: LALRAutomaton -> Set.Set LRNode
-- lalrAutomatonNodes automaton = Map.keysSet edges <> Set.fromList $
--   (Map.elems edges >>= Map.elems)
--   where edges = lalrAutomatonEdges automaton

lalrAutomatonNodes :: LALRAutomaton -> Set.Set LRNode
lalrAutomatonNodes (LALRAutomaton _ edges) = Map.keysSet edges

-- edges :: Map.Map n (Map.Map e n)
-- Map.toList edges :: [(n, Map.Map e n)]
-- traverse Map.toList :: (a, Map.Map b c) -> [(a, (b, c))]
-- Map.toList edges >>= traverse Map.toList :: [(n, (e, n))]

-- edges :: Map.Map n (t n)
-- Map.toList edges :: [(n, t n)]
-- Map.toList edges >>= sequence

reduces :: LALRAutomaton -> [([LRNode], String, LRNode)]
reduces automaton = do
  node            <- Set.toList (lalrAutomatonNodes automaton)
  LRItem n rn e r <- filter (\(LRItem n _ e r) -> n /= StartSymbol && e == r) (Set.toList node)
  return (path [] node e, rn, dst node (Left n))
  where dst start symbol = fromJust (Map.lookup start edges >>= Map.lookup symbol)
        path acc start []            = start : acc
        path acc start (symbol:rest) = path (start : acc) (dst start symbol) rest
        edges = lalrAutomatonEdges automaton

-------------------------------------------------------------------------------
