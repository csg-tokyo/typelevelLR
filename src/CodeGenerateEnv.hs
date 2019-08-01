
{-# LANGUAGE FlexibleContexts #-}

module CodeGenerateEnv (module CodeGenerateEnv) where

import Utility
import Syntax
import LALRAutomaton
import LRTable
import Debug.Trace

import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------

data CodeGenerateEnv = CodeGenerateEnv {
  codeGenerateEnvSyntax        :: Syntax,
  codeGenerateEnvAutomaton     :: LRAutomaton,
  codeGenerateEnvLRTable       :: LRTable,
  codeGenerateEnvNodeInfoTable :: NodeInfoTable
}

type NodeInfoTable = Map.Map LRNode NodeInfo

data NodeInfo = NodeInfo { nodeInfoName :: String,
                           nodeInfoType :: Symbol }

-------------------------------------------------------------------------------

buildCodeGenerateEnv :: Syntax -> LRAutomaton -> CodeGenerateEnv
buildCodeGenerateEnv syntax automaton = CodeGenerateEnv syntax automaton table nodeInfoTable
  where table       = lrTable automaton
        nodeInfoTable = buildNodeInfoTable automaton

buildNodeInfoTable :: LRAutomaton -> NodeInfoTable
buildNodeInfoTable automaton = Map.fromList $ do
  (i, node) <- zip [1 ..] (lrAutomatonNodes automaton)
  let nodeType = Map.findWithDefault (TerminalSymbol EndOfInput) node typeTable
  return (node, NodeInfo ("Node" ++ show i) nodeType)
  where typeTable = Map.fromListWith assertEq $ do
          (symbol, node) <- Map.elems (lrAutomatonEdgesTable automaton) >>= Map.toList
          return (node, symbol)

-------------------------------------------------------------------------------

syntax_ :: (MonadReader CodeGenerateEnv m) => m Syntax
syntax_ = asks codeGenerateEnvSyntax

automaton_ :: (MonadReader CodeGenerateEnv m) => m LRAutomaton
automaton_ = asks codeGenerateEnvAutomaton

lrTable_ :: (MonadReader CodeGenerateEnv m) => m LRTable
lrTable_ = asks codeGenerateEnvLRTable


nodeInfo_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m NodeInfo
nodeInfo_ node = asks ((Map.! node) . codeGenerateEnvNodeInfoTable)

nodeName_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m String
nodeName_ node = nodeInfoName <$> nodeInfo_ node

nodeType_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m Symbol
nodeType_ node = nodeInfoType <$> nodeInfo_ node


-- REMEMBER: inefficient algorithm
reducesFrom_ :: (MonadReader CodeGenerateEnv m) => LRNode -> Rule -> m [([LRNode], [LRNode])]
reducesFrom_ node rule = do
  automaton <- automaton_
  -- return [(reverse srcPath, reverse dstPath) |
  --         (srcPath, dstPath) <- reduces automaton rule,
  --         last srcPath == node]
  let result = [(reverse srcPath, reverse dstPath) |
                (srcPath, dstPath) <- reduces automaton rule,
                last srcPath == node]
  node_ <- nodeName_ node
  -- traceShow (node_, rule, length result) (return ())
  return result

-------------------------------------------------------------------------------

nodes_ :: (MonadReader CodeGenerateEnv m) => m [(LRNode, String, Symbol)]
nodes_ = do nodes <- lrAutomatonNodes <$> automaton_
            forM nodes (\node -> (,,) node <$> nodeName_ node <*> nodeType_ node)

nonTerminals_ :: (MonadReader CodeGenerateEnv m) => m [NonTerminal]
nonTerminals_ = syntaxNonTerminals <$> syntax_

ruleParams :: Rule -> [[String]]
ruleParams rule = [symbolParams symbol | symbol <- ruleRhs rule]

symbolParams :: Symbol -> [String]
symbolParams (NonTerminalSymbol nt) = [pascalCase (nonTerminalName nt)]
symbolParams (TerminalSymbol (UserTerminal _ params)) = params
symbolParams _  = []

nodeParams_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m [String]
nodeParams_ node = symbolParams <$> nodeType_ node

-------------------------------------------------------------------------------
