
{-# LANGUAGE FlexibleContexts #-}

module CodeGenerateEnv (module CodeGenerateEnv) where

import Utility
import Syntax
import LALRAutomaton

import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------

data CodeGenerateEnv = CodeGenerateEnv {
  codeGenerateEnvSyntax        :: Syntax,
  codeGenerateEnvAutomaton     :: LRAutomaton,
  codeGenerateEnvNodeInfoTable :: NodeInfoTable
}

type NodeInfoTable = Map.Map LRNode NodeInfo

data NodeInfo = NodeInfo { nodeInfoName :: String,
                           nodeInfoType :: Symbol }

-------------------------------------------------------------------------------

buildCodeGenerateEnv :: Syntax -> LRAutomaton -> CodeGenerateEnv
buildCodeGenerateEnv syntax automaton = CodeGenerateEnv syntax automaton nodeInfoTable
  where nodeInfoTable = buildNodeInfoTable automaton

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

nodeInfo_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m NodeInfo
nodeInfo_ node = asks ((Map.! node) . codeGenerateEnvNodeInfoTable)

nodeName_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m String
nodeName_ node = nodeInfoName <$> nodeInfo_ node

nodeType_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m Symbol
nodeType_ node = nodeInfoType <$> nodeInfo_ node

-------------------------------------------------------------------------------

nodes_ :: (MonadReader CodeGenerateEnv m) => m [(LRNode, String, Symbol)]
nodes_ = do nodes <- lrAutomatonNodes <$> automaton_
            forM nodes (\node -> (,,) node <$> nodeName_ node <*> nodeType_ node)

nonTerminals_ :: (MonadReader CodeGenerateEnv m) => m [NonTerminal]
nonTerminals_ = syntaxNonTerminals <$> syntax_

ruleParams :: Rule -> [String]
ruleParams rule = ruleRhs rule >>= symbolParams
  where symbolParams (NonTerminalSymbol nt) = return (pascalCase (nonTerminalName nt))
        symbolParams (TerminalSymbol (UserTerminal _ params)) = params

-------------------------------------------------------------------------------
