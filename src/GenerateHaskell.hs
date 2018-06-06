
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateHaskell (module GenerateHaskell) where

import Utility
import Syntax
import LALRAutomaton
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
import Data.List             (groupBy)
import Data.Function         (fix)

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellsLn (replicate 79 '-')

-------------------------------------------------------------------------------

tellHaskell :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellHaskell syntax = let automaton = lalrAutomaton syntax in
  (`runReaderT` buildCodeGenerateEnv syntax automaton) $ do
    let moduleName = pascalCase (syntaxName syntax)
    tellNewline
    tellLanguageOptions
    tellNewline
    tellsLn ("module " ++ moduleName ++ " where")
    tellNewline
    tellSeparator
    tellNewline
    sequenceWithSep_ (tellNewline >> tellSeparator >> tellNewline) $
      [tellGrammar,
       tellTypeProgramDefinition,
       tellASTDefinitions,
       tellTerminalMethodDefinitions, tellAutomatonStates,
       tellShiftTransitions,
       tellReduces, -- tellReduceTransitions,
       tellInitialState,
       tellEnd]
    tellNewline
    tellSeparator
    tellNewline

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
               m ()
tellGrammar = do
  syntax <- syntax_
  tellsLn "-- grammar definition"
  tellNewline
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "-- " >> tellRule rule >> tellNewline

-------------------------------------------------------------------------------

tellLanguageOptions :: (MonadWriter (Endo String) m) => m ()
tellLanguageOptions = do
  tellsLn "{-# LANGUAGE MultiParamTypeClasses #-}"
  tellsLn "{-# LANGUAGE FunctionalDependencies #-}"
  tellsLn "{-# LANGUAGE FlexibleInstances #-}"
  tellsLn "{-# LANGUAGE UndecidableInstances #-}"

tellTypeProgramDefinition :: (MonadWriter (Endo String) m) => m ()
tellTypeProgramDefinition = do
  tellsLn "type Program r a = (a -> r) -> r"
  tellNewline
  tellsLn "program :: a -> Program r a"
  tellsLn "program a = \\k -> k a"

-------------------------------------------------------------------------------

tellASTDefinitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                     m ()
tellASTDefinitions = do
  syntax <- syntax_
  tellsLn "-- AST nodes"
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    tellsLn ("data " ++ pascalCase (nonTerminalName nt))
    forM_ (zip ("  = " : repeat "  | ") (syntaxRules syntax nt)) $ \(header, rule) -> do
      tells header
      tells (pascalCase (ruleName rule))
      forM_ (ruleParams rule) $ \param ->
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
      tells ("  " ++ camelCase name ++ " :: s")
      forM_ params $ \param -> tells " -> " >> tells param
      tellsLn " -> Program r t"
    t -> error ("invalid terminal symbol found -- " ++ show t)

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

tellShiftTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                        m ()
tellShiftTransitions = do
  tellsLn "-- shift transitions"
  tellNewline
  automaton <- automaton_
  let shiftEdges = groupBy (equaling fst) (shifts automaton)
  forMWithSep_ (tellNewline >> tellNewline) shiftEdges $ \edges -> do
    forMWithSep_ tellNewline edges $ \(terminal, (src, dst)) -> do
      case terminal of
        UserTerminal name params -> do
          let className  = pascalCase name ++ "Transition"
          let methodName = camelCase name
          (srcName, dstName) <- (,) <$> nodeName_ src <*> nodeName_ dst
          tells ("instance {-# OVERLAPS #-} " ++ className ++ " ")
          tells ("(" ++ srcName ++ " prev) ")
          tellsLn ("(" ++ dstName ++ " (" ++ srcName ++ " prev)) where")
          tells ("  " ++ methodName ++ " prev")
          forM_ (zip [1 ..] params) $ \(i, param) -> tells (" x" ++ show i)
          tells (" = program (" ++ dstName ++ " prev")
          forM_ (zip [1 ..] params) $ \(i, param) -> tells (" x" ++ show i)
          tellsLn ")"
        _ -> error "unexpected EndOfInput"

-------------------------------------------------------------------------------

tellReduces :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
               m ()
tellReduces = do
  let getParams symbol = case symbol of
        NonTerminalSymbol nt -> [camelCase (nonTerminalName nt)]
        TerminalSymbol (UserTerminal name params) -> params
        TerminalSymbol EndOfInput                 -> []
  let tellNode nodes paramHandler = execStateT (loop nodes) 0
        where loop nodes = case nodes of
                []          -> tells "prev"
                node : rest -> do
                  NodeInfo name typ <- nodeInfo_ node
                  tells "(" >> tells name >> tells " " >> loop rest
                  forM_ (getParams typ) $ \paramType -> do
                    i <- modify (+ 1) >> get
                    paramHandler i paramType
                  tells ")"
  tellsLn "-- reduces"
  tellNewline
  tellsLn "class Reduce s t | s -> t where"
  tellsLn "  reduce :: s -> t"
  tellNewline
  nonTerminals <- syntaxNonTerminals <$> syntax_
  forM_ nonTerminals $ \nt -> do
    rules <- syntaxRules <$> syntax_ <*> pure nt
    forM_ rules $ \rule -> do
      tellNewline
      tells "-- " >> tellRule rule
      tellNewline
      reduceEdges <- reduces <$> automaton_ <*> pure rule
      forM_ reduceEdges $ \(src, dst) -> do
        tellNewline
        tells "instance Reduce "
        tellNode (reverse src) $ \_ _ -> return ()
        tells " "
        tellNode (reverse dst) $ \_ _ -> return ()
        tellsLn " where"

        tells "  reduce "
        nParams <- tellNode (init (reverse src)) $ \i _ -> tells " x" >> tellsShow i
        tells " = "
        tellNode (init (reverse dst)) $ \i _ -> case i of
          1 -> do
            tells " (" >> tells (pascalCase (ruleName rule))
            forM_ [1 .. nParams] $ \j ->
              tells " x" >> tellsShow j
            tells ")"
          _ -> error "broken reduce destination path"
        tellNewline
      when (not (null reduceEdges)) tellNewline

  tellNewline
  tellsLn "-- Reduce -> Transition"
  tellNewline
  terminals <- syntaxTerminals <$> syntax_
  forMWithSep_ tellNewline terminals $ \case
    UserTerminal name params -> do
      let className  = pascalCase name ++ "Transition"
      let methodName = camelCase  name
      tells   ("instance {-# OVERLAPS #-} (Reduce r s, " ++ className ++ " s t)")
      tellsLn (" => " ++ className ++ " r t where")
      tells   ("  " ++ methodName ++ " r")
      forM_ (zip [1 ..] params) $ \(i, _) -> tells " x" >> tellsShow i
      tells   (" = " ++ methodName ++ " (reduce r)")
      forM_ (zip [1 ..] params) $ \(i, _) -> tells " x" >> tellsShow i
      tellNewline
    _ -> error "unexpected EndOfInput"

-------------------------------------------------------------------------------

tellInitialState :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                    m ()
tellInitialState = do
  start    <- lrAutomatonStart <$> automaton_
  initName <- nodeName_ start
  tellsLn "-- initial state"
  tellNewline
  tellsLn ("begin :: Program r (" ++ initName ++ " ())")
  tellsLn ("begin = program (" ++ initName ++ " ())")

-------------------------------------------------------------------------------

tellEnd :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
           m ()
tellEnd = do
  tellsLn "class End s r | s -> r where"
  tellsLn "  end :: s -> r"
  tellNewline
  nodes <- map (\(node, _, _) -> node) <$> nodes_
  forMWithSep_ tellNewline (filter acceptible nodes) $ \node -> do
    NodeInfo name (NonTerminalSymbol nt) <- nodeInfo_ node
    tells    "instance {-# OVERLAPS #-} End"
    tells   (" (" ++ name ++ " prev)")
    tellsLn (" " ++ pascalCase (nonTerminalName nt) ++ " where")
    tellsLn ("  end (" ++ name ++ " _ r) = r")
  tellNewline
  tellsLn "instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where"
  tellsLn "  end r = end (reduce r)"

-------------------------------------------------------------------------------

