
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateScala where

import Utility
import Syntax
import LALRAutomaton
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
     tellShiftTransitions]
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
      forMWithSep_ (tells ", ") (zip [1 ..] (ruleParams rule)) $ \(i, param) -> do
        tells ("arg" ++ show i ++ " : " ++ param)
      tellsLn (" ) extends " ++ traitName)

-------------------------------------------------------------------------------

tellTransitionTraits :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => m ()
tellTransitionTraits = do
  syntax <- syntax_
  tellsLn "  // transitions"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      tellsLn ("  trait Transition" ++ pascalCase name ++ " [ Src, Dst ] {")
      tellsLn ("    def transit( src : Src ) : Dst")
      tellsLn "  }"
    t -> error ("invalid terminal symbol found -- " ++ show t)
  tellNewline
  tellNewline
  tellsLn "  // implicit classes for transition methods"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      tells ("  implicit class Transitable" ++ pascalCase name ++ " [ Src, Dst ] ")
      tellsLn ("( implicit t : Transition" ++ pascalCase name ++ "[ Src, Dst ] ) {")
      tellsLn ("    def " ++ name ++ "() : Dst = t.transit( src )")
      tellsLn "  }"
    t -> error ("invalid terminal symbol found -- " ++ show t)

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

tellShiftTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                        => m ()
tellShiftTransitions = do
  automaton <- automaton_
  tellsLn "  // shift transitions"
  tellNewline
  let shiftTransitions = groupBy (equaling fst) (shifts automaton)
  forMWithSep_ (tellNewline >> tellNewline) shiftTransitions $ \transitions -> do
    forMWithSep_ tellNewline transitions $ \(terminal, (src, dst)) -> do
      case terminal of
        UserTerminal name params -> do
          (srcName, dstName) <- (,) <$> nodeName_ src <*> nodeName_ dst
          let srcType = srcName ++ "[ Prev ]"
          let dstType = dstName ++ "[ " ++ srcName ++ "[ Prev ] ]"
          let transitionType = "Transition" ++ pascalCase name ++ "[ " ++ srcType ++ ", " ++ dstType ++ " ]"
          tells ("  implicit def shift_" ++ srcName ++ "_" ++ name)
          tellsLn (" [ Prev ] : " ++ transitionType ++ " = {")
          tellsLn ("    new " ++ transitionType ++ " {")
          tells ("      def transit( src : " ++ srcType ++ " ) : " ++ dstType)
          tellsLn (" = " ++ dstType ++ "( src )")
          tellsLn "    }"
          tellsLn "  }"

-------------------------------------------------------------------------------

tellReduceTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                      => m ()
tellReduceTransitions = do
  syntax    <- syntax_
  automaton <- automaton_
  tellsLn "  // reduce transitions"
  tellNewline
  forMWithSep_ (tellNewline >> tellNewline) (syntaxNonTerminals syntax) $ \nt -> do
    forMWithSep_ tellNewline (syntaxRules syntax nt) $ \rule -> do
      tells "  // " >> tellRule rule >> tellNewline
      tellNewline
      forMWithSep_ tellNewline (zip [1 .. ] (reduces automaton rule)) $ \(i, (src, dst)) -> do
        srcName <- nodeName_ (head src)
        tells ("  implicit def reduce_" ++ srcName ++ "_" ++ show i ++ "[Src, Dst]")
        tells ("(implicit t : ")

-- syntax :: Syntax
-- automaton :: LALRAutomaton
-- nt :: NonTerminal
-- rule :: Rule
-- src :: [LRNode]
-- dst :: [LRNode]

-------------------------------------------------------------------------------
