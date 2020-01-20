
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateTypeScript (module GenerateTypeScript) where

import Utility
import Syntax
import LALRAutomaton
import LRTable
import CodeGenerateEnv

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo())
import Control.Monad         (forM_)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe            (mapMaybe)
import Data.Either           (isRight)
import Data.List             (groupBy, mapAccumL)
import Data.Function         (fix)

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellsLn (replicate 79 '-')

-------------------------------------------------------------------------------

tellTypeScript :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellTypeScript syntax = let automaton = lalrAutomaton syntax in
  (`runReaderT` buildCodeGenerateEnv syntax automaton) $ do
    let moduleName = pascalCase (syntaxName syntax)
    tellNewline
    tellsLn ("namespace " ++ moduleName ++ " {")
    tellNewline
    tellSeparator
    tellNewline
    sequenceWithSep_ (tellNewline >> tellSeparator >> tellNewline) $
      [tellGrammar,
       tellASTDefinitions,
       tellTerminalMethodDefinitions,
       tellAutomatonStates,
       tellTransitions,
       tellInitialState]
    tellsLn ("}")
    tellNewline
    tellSeparator
    tellNewline

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
               m ()
tellGrammar = do
  syntax <- syntax_
  tellsLn "// grammar definition"
  tellNewline
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "// " >> tellRule rule >> tellNewline

-------------------------------------------------------------------------------

tellASTDefinitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                     m ()
tellASTDefinitions = do
  syntax <- syntax_
  tellsLn "// AST nodes"
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    tellsLn ("data " ++ pascalCase (nonTerminalName nt))
    forM_ (zip ("  = " : repeat "  | ") (syntaxRules syntax nt)) $ \(header, rule) -> do
      tells header
      tells (pascalCase (ruleName rule))
      forM_ (concat (ruleParams rule)) $ \param ->
        tells " " >> tells param
      tellNewline
    tellsLn "  deriving (Show)"

-------------------------------------------------------------------------------

tellTerminalMethodDefinitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                                 m ()
tellTerminalMethodDefinitions = do
  syntax <- syntax_
  tellsLn "-- terminal symbols"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      tellsLn ("class " ++ pascalCase name ++ "Transition s t | s -> t where")
      tells ("  " ++ camelCase name ++ " :: ")
      forM_ params $ \param -> tells param >> tells " -> "
      tellsLn "s -> t"
    t -> error ("invalid terminal symbol found -- " ++ show t)
  tellNewline
  tellsLn "class EndTransition s t | s -> t where"
  tellsLn "  end :: s -> t"

-------------------------------------------------------------------------------

tellAutomatonStates :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                       m ()
tellAutomatonStates = do
  nodes <- nodes_
  tellsLn "-- automaton states"
  tellNewline
  forMWithSep_ tellNewline nodes $ \(node, name, typ) -> do
    tells ("data " ++ name ++ " prev = " ++ name ++ " prev")
    case typ of
      NonTerminalSymbol nt -> tells " " >> tells (pascalCase (nonTerminalName nt))
      TerminalSymbol (UserTerminal name params) -> do
        forM_ params (\param -> tells " " >> tells param)
      _       -> return ()
    tellNewline

-------------------------------------------------------------------------------

tellTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                => m ()
tellTransitions = do
  table <- lrTable_
  tellsLn "-- transition instances"
  tellNewline
  forMWithSep_ tellNewline (lrTableTransitions table) $ \(src, t, action) -> do
    case action of
      Shift  dst  -> tellShiftTransition  src t dst
      Reduce rule -> tellReduceTransition src t rule
      Accept      -> tellAcceptTransition src

tellShiftTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                    => LRNode -> Terminal -> LRNode -> m ()
tellShiftTransition src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  dstName <- pascalCase <$> nodeName_ dst
  let className = pascalCase (terminalName t) ++ "Transition"
  let srcType = "(" ++ srcName ++ " prev)"
  let dstType = "(" ++ dstName ++ " (" ++ srcName ++ " prev))"
  let params = concat [" arg" ++ show i | (i, _) <- zip [1 ..] (terminalParams t)]
  tellsLn ("instance " ++ className ++ " " ++ srcType ++ " " ++ dstType ++ " where")
  tellsLn ("  " ++ camelCase (terminalName t) ++ params ++ " src = " ++ dstName ++ " src" ++ params)

tellReduceTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> Terminal -> Rule -> m ()
tellReduceTransition src t rule = do
  srcName <- pascalCase <$> nodeName_ src
  let pathToType path = case path of
        [] -> return "prev"
        node : rest -> do nodeName <- pascalCase <$> nodeName_ node
                          restType <- pathToType rest
                          return ("(" ++ nodeName ++ " " ++ restType ++ ")")
  reduces <- reducesFrom_ src rule
  forMWithSep_ tellNewline reduces $ \(srcPath, dstPath) -> do
    srcType <- pathToType srcPath
    dstType <- pathToType dstPath
    dstName <- pascalCase <$> nodeName_ (head dstPath)
    let className = pascalCase (terminalName t) ++ "Transition"
    let methodName = camelCase (terminalName t)
    let constraint = className ++ " " ++ dstType ++ " t"
    let (n, ipss) = mapAccumL (mapAccumL (\i a -> (i + 1, (i, a)))) 1 (ruleParams rule)
    let makeParamSrc ipsns = case ipsns of
          []                 -> return "prev"
          (ips, node) : rest -> do
            nodeName <- pascalCase <$> nodeName_ node
            rest'    <- makeParamSrc rest
            let params = concat [" arg" ++ show i | (i, _) <- ips]
            return ("(" ++ nodeName ++ " " ++ rest' ++ params ++ ")")
    paramSrc <- makeParamSrc (zip (reverse ipss) srcPath)
    let args = concat [" arg" ++ show i | i <- [1 .. n - 1]]
    let reductionRule = pascalCase (ruleName rule)
    let dst = "(" ++ dstName ++ " prev (" ++ reductionRule ++ args ++ "))"
    let params = concat [" p" ++ show i | (i, _) <- zip [1 ..] (terminalParams t)]
    tellsLn ("instance (" ++ constraint ++ ") => " ++ className ++ " " ++ srcType ++ " t where")
    tellsLn ("  " ++ methodName ++ params ++ " " ++ paramSrc ++ " = " ++ methodName ++ params ++ " " ++ dst)

tellAcceptTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> m ()
tellAcceptTransition src = do
  srcName <- pascalCase <$> nodeName_ src
  let srcType = "(" ++ srcName ++ " prev)"
  NonTerminalSymbol nt <- nodeType_ src
  let resultType = pascalCase (nonTerminalName nt)
  tellsLn ("instance EndTransition " ++ srcType ++ " " ++ resultType ++ " where")
  tellsLn ("  end (" ++ srcName ++ " _ arg1) = arg1")

-------------------------------------------------------------------------------

tellInitialState :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                 => m ()
tellInitialState = do
  automaton <- automaton_
  startName <- pascalCase <$> nodeName_ (lrAutomatonStart automaton)
  tellsLn ("begin :: " ++ startName ++ " ()")
  tellsLn ("begin = " ++ startName ++ " ()")

-------------------------------------------------------------------------------
