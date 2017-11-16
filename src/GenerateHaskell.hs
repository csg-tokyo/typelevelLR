
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateHaskell (module GenerateHaskell) where

import Utility
import Syntax
import LALRAutomaton

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo())
import Control.Monad         (forM_)
import Data.Function         (fix)
import Control.Monad.Writer  (MonadWriter())

import Data.Either           (isRight)

-------------------------------------------------------------------------------

newline :: (MonadWriter (Endo String) m) => m ()
newline = tells "\n"

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellsLn (replicate 79 '-')

-------------------------------------------------------------------------------

tellHaskell :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellHaskell syntax = do
  let moduleName = pascalCase (syntaxName syntax)
  let automaton = lalrAutomaton syntax
  let nodeInfo = buildNodeInfoTable automaton
  newline
  tellLanguageOptions
  newline
  tellsLn ("module " ++ moduleName ++ " where")
  newline
  tellSeparator
  newline
  tellGrammar syntax
  newline
  tellSeparator
  newline
  tellTypeProgramDefinition
  newline
  tellSeparator
  newline
  tellASTDefinition syntax
  newline
  tellSeparator
  newline
  tellTerminalMethodDefinitions syntax
  newline
  newline
  tellAutomatonStates nodeInfo
  newline
  tellSeparator
  newline
  tellShiftTransitions automaton nodeInfo
  newline
  tellSeparator
  newline
  tellReduces automaton nodeInfo
  newline
  newline
  tellReduceTransitions syntax
  newline
  tellSeparator
  newline
  tellInitialState automaton nodeInfo
  newline
  tellSeparator
  newline
  tellEnd automaton nodeInfo
  newline
  tellSeparator
  newline

-------------------------------------------------------------------------------

data NodeInfo = NodeInfo { nodeName :: String,
                           nodeType :: Symbol }
  deriving (Show)

buildNodeInfoTable :: LALRAutomaton -> Map.Map LRNode NodeInfo
buildNodeInfoTable automaton = Map.fromList $ do
  node <- Set.toList (lalrAutomatonNodes automaton)
  let name = nodeName Map.! node
  let typ  = Map.lookup node nodeType `orElse` Right EndOfInput
  return (node, NodeInfo name typ)
  where nodeName = buildNodeNameTable automaton
        nodeType = buildNodeTypeTable automaton

buildNodeNameTable :: LALRAutomaton -> Map.Map LRNode String
buildNodeNameTable automaton = Map.fromList $
  [(node, 'S' : show i) | (i, node) <- zip [1 ..] (Set.toList nodes)]
  where nodes = lalrAutomatonNodes automaton

buildNodeTypeTable :: LALRAutomaton -> Map.Map LRNode Symbol
buildNodeTypeTable automaton = Map.fromListWithKey merge $
  [(node, symbol) | (symbol, node) <- Map.elems edges >>= Map.toList]
  where merge node a b = if a == b then a else error ("ambiguous node type for " ++ show node)
        edges = lalrAutomatonEdges automaton

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellGrammar syntax = do
  tellsLn "-- grammar definition"
  newline
  forM_ (nonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \(name, expr) -> do
      tells ("-- " ++ name ++ " : " ++ nonTerminalName nt ++ " -> ")
      tellExpr expr
      newline

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
  newline
  tellsLn "program :: a -> Program r a"
  tellsLn "program a = \\k -> k a"

tellASTDefinition :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTDefinition syntax = do
  tellsLn "-- AST nodes"
  forM_ (nonTerminals syntax) $ \nt -> do
    newline
    tellsLn ("data " ++ typeName nt)
    forM_ (zip ("  = " : repeat "  | ") (syntaxRules syntax nt)) $ \(header, (name, expr)) -> do
      tells header
      tells (pascalCase name)
      forM_ expr $ \case
        Left  nt'           -> tells (" " ++ typeName nt')
        Right StringLiteral -> tells " String"
        Right IntLiteral    -> tells " Integer"
        _                   -> return ()
      newline
    tellsLn "  deriving (Show)"

-------------------------------------------------------------------------------

tellTerminalMethodDefinitions :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellTerminalMethodDefinitions syntax = do
  tellsLn "-- terminal symbols"
  newline
  forMWithSep_ newline (terminals syntax) $ \case
    Keyword key -> do
      let methodName = camelCase key
      let className = pascalCase methodName ++ "Transition"
      tellsLn ("class " ++ className ++ " s t | s -> t where")
      tellsLn ("  " ++ methodName ++ " :: s -> Program r t")
    StringLiteral ->  do
      tellsLn "class StrTransition s t | s -> t where"
      tellsLn "  str :: s -> String -> Program r t"
    IntLiteral -> do
      tellsLn "class IntTransition s t | s -> t where"
      tellsLn "  int :: s -> Integer -> Program r t"
    t -> error ("invalid terminal symbol found -- " ++ show t)

tellAutomatonStates :: (MonadWriter (Endo String) m) =>
                       Map.Map LRNode NodeInfo -> m ()
tellAutomatonStates nodeInfo = do
  tellsLn "-- automaton states"
  newline
  forMWithSep_ newline (Map.toList nodeInfo) $ \(node, NodeInfo name typ) -> do
    tells ("data " ++ name ++ " prev = " ++ name ++ " prev")
    case typ of
      Left nt -> tells (" " ++ typeName nt)
      Right StringLiteral -> tells " String"
      Right IntLiteral    -> tells " Integer"
      _       -> return ()
    newline

tellShiftTransitions :: (MonadWriter (Endo String) m) =>
                        LALRAutomaton -> Map.Map LRNode NodeInfo -> m ()
tellShiftTransitions automaton nodeInfo = do
  tellsLn "-- shift transitions"
  newline
  let edges = lalrAutomatonEdges automaton
  let transitions = filter (not . null . snd) $ do
        (src, dsts) <- Map.toList edges
        return (src, filter (isRight . fst) (Map.toList dsts))
  forMWithSep_ (newline >> newline) transitions $ \(src, dsts) -> do
    forMWithSep_ newline dsts $ \(symbol, dst) -> do
      let srcName = nodeName (nodeInfo Map.! src)
      let dstName = nodeName (nodeInfo Map.! dst)
      let (methodName, methodArg) = case symbol of
            Right (Keyword key) -> (camelCase key, "")
            Right StringLiteral -> ("str", " s")
            Right IntLiteral    -> ("int", " i")
            -- _                   -> (False, undefined, undefined)
      let className = pascalCase methodName ++ "Transition"
      tells ("instance {-# OVERLAPS #-} " ++ className ++ " ")
      tells ("(" ++ srcName ++ " prev) ")
      tellsLn ("(" ++ dstName ++ " (" ++ srcName ++ " prev)) where")
      tells ("  " ++ methodName ++ " prev" ++ methodArg ++ " = ")
      tellsLn ("program (" ++ dstName ++ " prev" ++ methodArg ++ ")")

tellReduces :: (MonadWriter (Endo String) m) =>
               LALRAutomaton -> Map.Map LRNode NodeInfo -> m ()
tellReduces automaton nodeInfo = do
  tellsLn "-- reduces"
  newline
  tellsLn "class Reduce s t | s -> t where"
  tellsLn "  reduce :: s -> t"
  newline
  newline
  forMWithSep_ newline (reduces automaton) $ \(src, ruleName, dst) -> do
    let baseName = nodeName (nodeInfo Map.! last src)
    tells "instance Reduce"
    flip fix src $ \loop ss -> case ss of
      []      -> tells " prev"
      s : ss' -> let name = nodeName (nodeInfo Map.! s) in
        tells " (" >> tells name >> loop ss' >> tells ")"
    let dstName = nodeName (nodeInfo Map.! dst)
    tellsLn (" (" ++ dstName ++ " (" ++ baseName ++ " prev)) where")
    tells "  reduce"
    nParam <- flip fix (init src) $ \loop ss -> case ss of
      []      -> 0 <$ tells " prev"
      s : ss' -> do
        let NodeInfo name typ = nodeInfo Map.! s
        tells (" (" ++ name)
        i <- loop ss'
        let hasParam = case typ of
              Left  _             -> True
              Right StringLiteral -> True
              Right IntLiteral    -> True
              _                   -> False
        if hasParam
          then (i + 1) <$ tells (" p" ++ show (i + 1) ++ ")")
          else i       <$ tells ")"
    tells (" = " ++ dstName ++ " prev (" ++ pascalCase ruleName)
    forM_ [1 .. nParam] $ \i -> tells (" p" ++ show i)
    tellsLn ")"

-------------------------------------------------------------------------------

tellReduceTransitions :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellReduceTransitions syntax = do
  tellsLn "-- Reduce -> Transition"
  newline
  forMWithSep_ newline (terminals syntax) $ \case
    Keyword key -> do
      let methodName = camelCase key
      let className = pascalCase methodName ++ "Transition"
      tellsLn ("instance {-# OVERLAPS #-} (Reduce r s, " ++ className ++ " s t) =>")
      tellsLn ("    " ++ className ++ " r t where")
      tellsLn ("  " ++ methodName ++ " r = " ++ methodName ++ " (reduce r)")
    StringLiteral -> do
      tellsLn "instance {-# OVERLAPS #-} (Reduce r s, StrTransition s t) =>"
      tellsLn "    StrTransition r t where"
      tellsLn "  str r s = str (reduce r) s"
    IntLiteral -> do
      tellsLn "instance {-# OVERLAPS #-} (Reduce r s, IntTransition s t) =>"
      tellsLn "    IntTransition r t where"
      tellsLn "  int r s = int (reduce r) s"

-------------------------------------------------------------------------------

tellInitialState :: (MonadWriter (Endo String) m) =>
                    LALRAutomaton -> Map.Map LRNode NodeInfo -> m ()
tellInitialState automaton nodeInfo = do
  let initName = nodeName (nodeInfo Map.! lalrAutomatonStart automaton)
  tellsLn "-- initial state"
  newline
  tellsLn ("begin :: Program r (" ++ initName ++ " ())")
  tellsLn ("begin = program (" ++ initName ++ " ())")

-------------------------------------------------------------------------------

tellEnd :: (MonadWriter (Endo String) m) =>
           LALRAutomaton -> Map.Map LRNode NodeInfo -> m ()
tellEnd automaton nodeInfo = do
  tellsLn "class End s r | s -> r where"
  tellsLn "  end :: s -> r"
  newline
  let nodes = Set.toList (lalrAutomatonNodes automaton)
  let endable (LRItem nt _ _ rest) = nt == StartSymbol && null rest
  forMWithSep_ newline (filter (any endable) nodes) $ \node -> do
    let NodeInfo name (Left nt) = nodeInfo Map.! node
    tells "instance {-# OVERLAPS #-} End"
    tells (" (" ++ name ++ " prev)")
    tellsLn (" " ++ pascalCase (nonTerminalName nt) ++ " where")
    tellsLn ("  end (" ++ name ++ " _ r) = r")
  newline
  tellsLn "instance {-# OVERLAPS #-} (Reduce r s, End s t) => End r t where"
  tellsLn "  end r = end (reduce r)"

-------------------------------------------------------------------------------

typeName :: NonTerminal -> String
typeName nt = pascalCase (nonTerminalName nt)

varName :: NonTerminal -> String
varName nt = camelCase (nonTerminalName nt)

-------------------------------------------------------------------------------
