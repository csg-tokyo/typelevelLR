
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateScala where

import Utility
import Syntax
import LALRAutomaton
import LRTable
import CodeGenerateEnv

import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellNewline >> tellsLn ("  " ++ replicate 117 '/') >> tellNewline

tellScala :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => m ()
tellScala = do
  syntax <- syntax_
  let moduleName = syntaxName syntax
  tellNewline
  tellsLn "import scala.language.implicitConversions"
  tellNewline >> tellsLn (replicate 119 '/') >> tellNewline
  tellGrammar
  tellNewline >> tellsLn (replicate 119 '/') >> tellNewline
  -- tellAutomaton
  -- tellNewline >> tellsLn (replicate 179 '/') >> tellNewline
  tellsLn ("object " ++ moduleName ++ " {")
  tellSeparator
  sequenceWithSep_ tellSeparator $
    [tellASTDefinition,
     tellTransitionTraits,
     tellStackElements,
     tellTransitionImpls,
     tellBegin]
  tellSeparator
  tellsLn "}"
  tellNewline

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
            => m ()
tellGrammar = do
  syntax <- syntax_
  tellsLn "// grammar"
  tellNewline
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "// " >> tellRule rule >> tellNewline

-------------------------------------------------------------------------------

tellASTDefinition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                  => m ()
tellASTDefinition = do
  syntax <- syntax_
  tellsLn "  // AST"
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    let traitName = pascalCase (nonTerminalName nt)
    tellsLn ("  trait " ++ traitName)
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tells ("  case class " ++ className ++ " ( ")
      forMWithSep_ (tells ", ") (zip [1 ..] (concat (ruleParams rule))) $ \(i, param) -> do
        tells ("arg" ++ show i ++ " : " ++ param)
      tellsLn (" ) extends " ++ traitName)

-------------------------------------------------------------------------------

tellTransitionTraits :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => m ()
tellTransitionTraits = do
  syntax <- syntax_
  tellsLn "  // transition type classes"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      let ps = concat [", arg" ++ show i ++ " : " ++ param | (i, param) <- zip [1 ..] params]
      tellsLn ("  trait " ++ pascalCase name ++ "Transition [ Src, Dst ] {")
      tellsLn ("    def transit( src : Src"  ++ ps ++ " ) : Dst")
      tellsLn "  }"
    t -> error ("invalid terminal symbol found -- " ++ show t)
  tellNewline
  tellEndTransitionTrait
  tellNewline
  tellNewline
  tellsLn "  // implicit classes for transition methods"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      let className = pascalCase name ++ "Transitable"
      let implicitType = pascalCase name ++ "Transition[ Src, Dst ]"
      let paramsSeq = intercalate ", " ["arg" ++ show i ++ " : " ++ param | (i, param) <- zip [1 ..] params]
      let argsSeq = concat [", arg" ++ show i | (i, _) <- zip [1 ..] params]
      tells ("  implicit class " ++ className ++ " [ Src, Dst ] ( src : Src ) ")
      tellsLn ("( implicit t : " ++ implicitType ++ " ) {")
      tellsLn ("    def " ++ name ++ "( " ++ paramsSeq ++ " ) : Dst = {")
      tellsLn ("      t.transit( src" ++ argsSeq ++ " )")
      tellsLn "    }"
      tellsLn "  }"
    t -> error ("invalid terminal symbol found -- " ++ show t)
  tellNewline
  tellEndTransitable


tellEndTransitionTrait :: (MonadWriter (Endo String) m) => m ()
tellEndTransitionTrait = do
  tellsLn "  trait EndTransition [ Src, Dst ] {"
  tellsLn "    def transit( src : Src ) : Dst"
  tellsLn "  }"

tellEndTransitable :: (MonadWriter (Endo String) m) => m ()
tellEndTransitable = do
  tellsLn "  implicit class EndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EndTransition[ Src, Dst ] ) {"
  tellsLn "    def end() : Dst = t.transit( src )"
  tellsLn "  }"

-------------------------------------------------------------------------------

tellStackElements :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                  => m ()
tellStackElements = do
  nodes <- nodes_
  tellsLn "  // stack elements"
  tellNewline
  forMWithSep_ tellNewline nodes $ \(node, name, typ) -> do
    let params = case typ of
          NonTerminalSymbol nt                   -> [pascalCase (nonTerminalName nt)]
          TerminalSymbol (UserTerminal _ params) -> map pascalCase params
          _                                      -> []
    tells ("  case class " ++ pascalCase name ++ " [ Prev ] ( prev : Prev")
    forM_ (zip [1 ..] params) $ \(i, param) -> do
      tells (", arg" ++ show i ++ " : " ++ param)
    tellsLn " )"

-------------------------------------------------------------------------------

tellTransitionImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                => m ()
tellTransitionImpls = do
  table <- lrTable_
  tellsLn "  // transition implementations"
  tellNewline
  forMWithSep_ tellNewline (lrTableTransitions table) $ \(src, t, action) -> do
    case action of
      Shift  dst  -> tellShiftTransitionImpl  src t dst
      Reduce rule -> tellReduceTransitionImpl src t rule
      Accept      -> tellAcceptTransitionImpl src


tellShiftTransitionImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                        => LRNode -> Terminal -> LRNode -> m ()
tellShiftTransitionImpl src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  dstName <- pascalCase <$> nodeName_ dst
  case t of
    EndOfInput -> error "impossible occurs: an EndOfInput is given to tellShiftTransitionImpl"
    UserTerminal name params -> do
      let methodName = "shift" ++ srcName ++ pascalCase name
      let srcType = srcName ++ "[ Prev ]"
      let dstType = dstName ++ "[ " ++ srcType ++ " ]"
      let transitionType = pascalCase name ++ "Transition[ " ++ srcType ++ ", " ++ dstType ++ " ]"
      let paramsSeq = concat [", arg" ++ show i ++ " : " ++ pascalCase param | (i, param) <- zip [1 ..] params]
      let argsSeq = concat [", arg" ++ show i | (i, _) <- zip [1 ..] params]
      tellsLn ("  implicit def " ++ methodName ++ "[ Prev ] : " ++ transitionType ++ " = {")
      tellsLn ("    new " ++ transitionType ++ " {")
      tellsLn ("      def transit( src : " ++ srcType ++ paramsSeq ++ " ) : " ++ dstType ++ " = {")
      tellsLn ("        " ++ dstType ++ "( src" ++ argsSeq ++ " )")
      tellsLn "      }"
      tellsLn "    }"
      tellsLn "  }"

tellReduceTransitionImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                         => LRNode -> Terminal -> Rule -> m ()
tellReduceTransitionImpl src t rule = do
  srcName <- pascalCase <$> nodeName_ src
  let tName = terminalName t
  let pathToType path = case path of
        []          -> return "Prev"
        node : rest -> do nodeName <- pascalCase <$> nodeName_ node
                          restType <- pathToType rest
                          return (nodeName ++ "[ " ++ restType ++ " ]")
  reduces <- reducesFrom_ src rule
  forM_ (zip [1 ..] reduces) $ \(i, (srcPath, dstPath)) -> do
    srcType <- pathToType srcPath
    dstType <- pathToType dstPath
    let methodName = "reduce" ++ srcName ++ pascalCase tName ++ show i
    let implicitType = pascalCase tName ++ "Transition[ " ++ dstType ++ ", Dst ]"
    let transitionType = pascalCase tName ++ "Transition[ " ++ srcType ++ ", Dst ]"
    let paramsSeq = concat [", arg" ++ show i ++ " : " ++ param | (i, param) <- zip [1 ..] (terminalParams t)]
    let argsSeq = concat [", arg" ++ show i | (i, _) <- zip [1 ..] (terminalParams t)]
    let reductionRule = pascalCase (ruleName rule)
    let reductionArgs = let
            pss = ruleParams rule
            ps = ["src" ++ concat (replicate (length pss - i) ".prev") ++ ".arg" ++ show j |
                  (i, ps) <- zip [1 ..] pss, (j, p) <- zip [1 ..] ps]
          in intercalate ", " ps
    tellsLn ("  implicit def " ++ methodName ++ "[ Prev, Dst ] ( implicit t : " ++ implicitType ++ " ) : " ++ transitionType ++ " = {")
    tellsLn ("    new " ++ transitionType ++ " {")
    tellsLn ("      def transit( src : " ++ srcType ++ paramsSeq ++ " ) : Dst = {")
    tellsLn ("        val prev = src" ++ concat (".prev" <$ ruleRhs rule))
    tellsLn ("        val tree = " ++ reductionRule ++ "( " ++ reductionArgs ++ " )")
    tellsLn ("        t.transit( " ++ dstType ++ "( prev, tree )" ++ argsSeq ++ " )")
    tellsLn "      }"
    tellsLn "    }"
    tellsLn "  }"

tellAcceptTransitionImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                         => LRNode -> m ()
tellAcceptTransitionImpl src = do
  srcName              <- pascalCase <$> nodeName_ src
  let methodName = "accept" ++ srcName
  let srcType = srcName ++ "[ Prev ]"
  NonTerminalSymbol nt <- nodeType_ src
  let resultType = pascalCase (nonTerminalName nt)
  let transitionType = "EndTransition[ " ++ srcType ++ ", " ++ resultType ++ " ]"
  tellsLn ("  implicit def " ++ methodName ++ "[ Prev ] : " ++ transitionType ++ " = {")
  tellsLn ("    new " ++ transitionType ++ " {")
  tellsLn ("      def transit( src : " ++ srcType ++ " ) : " ++ resultType ++ " = {")
  tellsLn "        src.arg1"
  tellsLn "      }"
  tellsLn "    }"
  tellsLn "  }"

-------------------------------------------------------------------------------

tellBegin :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
          => m ()
tellBegin = do
  startNode <- lrAutomatonStart <$> automaton_
  startName <- pascalCase <$> nodeName_ startNode
  tellsLn ("  def begin() : " ++ startName ++ "[ Unit ] = " ++ startName ++ "( () )")

-------------------------------------------------------------------------------
