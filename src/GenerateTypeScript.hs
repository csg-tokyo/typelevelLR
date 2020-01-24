
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
import Data.List             (intercalate)

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
tellSeparator = tellsLn (replicate 79 '/')

-------------------------------------------------------------------------------

tellTypeScript :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellTypeScript syntax = let automaton = lalrAutomaton syntax in
  (`runReaderT` buildCodeGenerateEnv syntax automaton) $ do
    let moduleName = pascalCase (syntaxName syntax)
    tellNewline
    tellSeparator
    tellNewline
    sequenceWithSep_ (tellNewline >> tellSeparator >> tellNewline) $
      [tellGrammar,
       tellUtils,
       tellASTDefinitions,
       tellAutomatonStates,
       tellTransitions,
       tellInitialState]
    tellNewline
    tellSeparator
    tellNewline

-------------------------------------------------------------------------------

tellUtils :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
             m ()
tellUtils = do
  tellsLn "// util scripts"
  tellNewline
  tellsLn utilScripts

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
  -- AST abstract classes
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    tellsLn $ "interface " ++ pascalCase (nonTerminalName nt) ++ " {"
    tellsLn "\taccept(v? : Visitor): void"
    tellsLn "}"

  tellNewline

  -- AST concrete classes
  forMWithSep_ (tellNewline) (syntaxNonTerminals syntax) $ \nt -> do
    forMWithSep_ (tellNewline) (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (concat (ruleParams rule))]
      tellsLn $ "export class " ++ className ++ " implements " ++ pascalCase (nonTerminalName nt) ++ " {"
      forM_ args $ \(argType, argName) -> do
        tellsLn $ "\t" ++ argName ++ " : " ++ argType
      case args of
        [] -> return ()
        _  -> do
          tells "\tconstructor("
          forMWithSep_ (tells ", ") args $ \(argType, argName) -> do
            tells $ argName ++ " : " ++ argType
          tellsLn ") {"
          forM_ args $ \(argType, argName) -> do
            tellsLn $ "\t\tthis." ++ argName ++ " = " ++ argName
          tellsLn "\t}"
      tellsLn "\taccept(v? : Visitor) {"
      tellsLn "\t\tif (v) {"
      tellsLn $ "\t\t\tv.visit" ++ className ++ "(this)"
      tellsLn "\t\t} else {"
      tellsLn $ "\t\t\tnew DefaultVisitor().visit" ++ className ++ "(this)"
      tellsLn "\t\t}"
      tellsLn "\t}"
      tellsLn "}"

  tellNewline

  -- AST visitors
  tellsLn "interface Visitor {"
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tellsLn $ "\tvisit" ++ className ++ "(host : " ++ className ++ "): void"
  tellsLn "}"

  tellNewline

  tellsLn "export class DefaultVisitor implements Visitor {"
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tellsLn $ "\tvisit" ++ className ++ "(host : " ++ className ++ ") {"
      tellsLn $ "\t\tprocess.stdout.write(\"" ++ className ++ "(\")"
      forM_ (zip [1 ..] (concat $ ruleParams rule)) $ \(i, typ) -> do
        -- tellsLn $ concat $ map nonTerminalName $ syntaxNonTerminals syntax
        if elem typ $ map (pascalCase . nonTerminalName) $ syntaxNonTerminals syntax
        then
          tellsLn ("\t\thost.arg" ++ show i ++ ".accept(this)")
        else
          tellsLn ("\t\tprocess.stdout.write(host.arg" ++ show i ++ ")")
      tellsLn $ "\t\tprocess.stdout.write(\")\")"
      tellsLn "\t}"
  tellsLn "}"

-------------------------------------------------------------------------------

tellAutomatonStates :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => m ()
tellAutomatonStates = do
  nodes <- nodes_
  tellsLn "// automaton states"
  tellNewline
  tells "type Node = "
  tellsLn $ intercalate " | " $ map (\(node, name, typ) -> name) nodes
  tellNewline
  forMWithSep_ tellNewline nodes $ \(node, name, typ) -> do
    tellsLn $ "class " ++ name ++ " {"
    tellsLn $ "\tprivate _" ++ name ++ "Brand: boolean = true"
    params <- nodeParams_ node
    case params of
      [] -> return ()
      _  -> do
        forM_ (zip [1 ..] params) $ \(i, param) -> do
          tellsLn ("\targ" ++ show i ++ " : " ++ param)
        tells ("\tconstructor(")
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells ("arg" ++ show i ++ " : " ++ param)
        tellsLn ") {"
        forM_ (zip [1 ..] params) $ \(i, param) -> do
          tellsLn ("\t\tthis.arg" ++ show i ++ " = " ++ "arg" ++ show i)
        tellsLn "\t}"
    tellsLn "}"

-------------------------------------------------------------------------------

tellTransitions ::  (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => m ()
tellTransitions = do
  table <- lrTable_
  tellsLn "// transitions"
  tellNewline
  -- type guards
  forMWithSep_ tellNewline (lrTableTransitions table) $ \(src, t, action) -> do
    case action of
      Shift  dst  -> do
        srcName <- pascalCase <$> nodeName_ src
        tellTypeGuards [srcName]
      Reduce rule -> do
        reduces <- reducesFrom_ src rule
        forMWithSep_ tellNewline reduces $ \(srcPath, dstPath) -> do
          path <- mapM nodeName_ srcPath
          tellTypeGuards path
      Accept      -> do
        srcName <- nodeName_ src
        tellTypeGuards [srcName]
  -- fluent type
  tellsLn "type Fluent<Stack extends unknown[]> = ("
  forMWithSep_ (tellsLn ") & (") (lrTableTransitions table) $ \(src, t, action) -> do
    case action of
      Shift  dst  -> tellShiftFluentType  src t dst
      Reduce rule -> tellReduceFluentType src t rule
      Accept      -> tellAcceptFluentType src
  tellsLn ")"
  -- fluent implementation
  -- forMWithSep_ tellNewline (lrTableTransitions table) $ \(src, t, action) -> do
  --   case action of
  --     Shift  dst  -> tellShiftTransition  src t dst
  --     Reduce rule -> tellReduceTransition src t rule
  --     Accept      -> tellAcceptTransition src

tellReduceFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> Terminal -> Rule -> m ()
tellReduceFluentType src t rule = do
  reduces <- reducesFrom_ src rule
  forMWithSep_ tellNewline reduces $ \(srcPath, dstPath) -> do
    condition <- do path <- mapM nodeName_ srcPath
                    return $ "\tStartsWith<Stack, [" ++ (intercalate ", " path) ++ "]> extends 1 ?"
    dstType <- do path <- mapM nodeName_ dstPath
                  return $ "Fluent<AddUnknownRest<[" ++ (intercalate ", " path) ++ "]>>"
    tellsLn condition
    let funName = terminalName t
    let params = terminalParams t
    let args = intercalate ", " ["arg" ++ show i ++ ": " ++ typ | (i, typ) <- zip [1 ..] params]
    tellsLn $ "\t\t{ " ++ funName ++ ": (" ++ args ++ ") => ReturnType<" ++ dstType ++ "['" ++ funName ++ "']> } :"
    tellsLn "\t\t{}"

tellShiftFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                    => LRNode -> Terminal -> LRNode -> m ()
tellShiftFluentType src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  tellsLn $ "\tStartsWith<Stack, [" ++ srcName ++ "]> extends 1 ?"
  dstName <- pascalCase <$> nodeName_ dst
  let funName = terminalName t
  let dstType = "Fluent<AddUnknownRest<Prepend<" ++ dstName ++ ", Stack>>>"
  let params = terminalParams t
  let args = intercalate ", " ["arg" ++ show i ++ ": " ++ typ | (i, typ) <- zip [1 ..] params]
  tellsLn $ "\t\t{ " ++ funName ++ ": (" ++ args ++ ") => " ++ dstType ++ " } :"
  tellsLn "\t\t{}"

tellAcceptFluentType :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> m ()
tellAcceptFluentType src = do
  srcName <- nodeName_ src
  tellsLn $ "\tStartsWith<Stack, [" ++ srcName ++ "]> extends 1 ?"
  tellsLn $ "\t\t{ end: () => " ++ srcName ++ "['arg1']" ++ " } :"
  tellsLn "\t\t{}"

tellTypeGuards ::  (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) => [String] -> m ()
tellTypeGuards nodes = do
  tellsLn $ "function startsWith" ++ (concat nodes) ++ "(arg: any): arg is AddUnknownRest<[" ++ (intercalate ", " nodes) ++ "]> {"
  tells "\treturn "
  forMWithSep_ (tells "\t\t&& ") (zip [0 ..] nodes) $ \(i, node) -> do
    tellsLn $ "arg[" ++ show i ++ "] && arg[" ++ show i ++ "]._" ++ node ++ "Brand"
  tellsLn "}"
  tellNewline

tellShiftTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                    => LRNode -> Terminal -> LRNode -> m ()
tellShiftTransition src t dst = do
  srcName <- pascalCase <$> nodeName_ src
  dstName <- pascalCase <$> nodeName_ dst
  let funName = terminalName t ++ "_transition"
  let srcType = "State< " ++ srcName ++ ", Tail... >"
  let dstType = "State< " ++ dstName ++ ", " ++ srcName ++ ", Tail... >"
  let params = terminalParams t
  let paramList = srcType ++ " const& src" ++ concat [", " ++ typ ++ " const& arg" ++ show i | (i, typ) <- zip [1 ..] params]
  let dstArgs = intercalate ", " ["arg" ++ show i | (i, _) <- zip [1 ..] params]
  tellsLn "template< typename... Tail >"
  tellsLn ("auto " ++ funName ++ "( " ++ paramList ++ " ) {")
  tellsLn ("  return " ++ dstType ++ "::make( " ++ dstName ++ "( " ++ dstArgs ++ " ), src );")
  tellsLn "}"

tellReduceTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> Terminal -> Rule -> m ()
tellReduceTransition src t rule = do
  reduces <- reducesFrom_ src rule
  forMWithSep_ tellNewline reduces $ \(srcPath, dstPath) -> do
    srcType <- do path <- mapM nodeName_ srcPath
                  return ("State< " ++ concat [name ++ ", " | name <- path] ++ "Tail... >")
    dstType <- do path <- mapM nodeName_ dstPath
                  return ("State< " ++ concat [name ++ ", " | name <- path] ++ "Tail... >")
    baseType <- do baseName <- nodeName_ (last dstPath)
                   return ("State< " ++ baseName ++ ", Tail... >")
    let funName = terminalName t ++ "_transition"
    let params = terminalParams t
    let paramList = " const& src" ++ concat [", " ++ typ ++ " const& arg" ++ show i | (i, typ) <- zip [1 ..] params]
    dstName <- pascalCase <$> nodeName_ (head dstPath)
    let contentType = (nonTerminalName (ruleLhs rule))
    tellsLn "template< typename... Tail >"
    tellsLn ("auto " ++ funName ++ "( " ++ paramList ++ " ) {")
    n <- (`execStateT` 0) $ forM_ (zip [1 ..] (ruleRhs rule)) $ \(i, sym) -> case sym of
      NonTerminalSymbol nt -> do
        j <- modify (+ 1) >> get
        tellsLn ("  " ++ (nonTerminalName nt) ++ " const& x" ++ show j ++ " = src->" ++ ([1 .. length (ruleRhs rule) - i] *> "tail->") ++ "head.arg1;")
      TerminalSymbol t -> do
        forM_ (zip [1 ..] (terminalParams t)) $ \(k, param) -> do
          j <- modify (+ 1) >> get
          tellsLn ("  " ++ param ++ " const& x" ++ show j ++ " = src->" ++ ([1 .. length (ruleRhs rule) - i] *> "tail->") ++ "head.arg" ++ show k ++ ";")
    tellsLn ("  " ++ contentType ++ " const& content = " ++ contentType ++ "( new " ++ ruleName rule ++ "( " ++ intercalate ", " ["x" ++ show i | i <- [1 .. n]] ++ " ) );")
    tellsLn ("  " ++ baseType ++ " const& tail = src" ++ concat ["->tail" | _ <- tail srcPath] ++ ";")
    tellsLn ("  return " ++ funName ++ "( " ++ dstType ++ "::make( " ++ dstName ++ "( content ), tail )" ++ concat [", arg" ++ show i | (i, _) <- zip [1 ..] params] ++ " );")
    tellsLn "}"

tellAcceptTransition :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => LRNode -> m ()
tellAcceptTransition src = do
  srcName <- nodeName_ src
  let srcType = "State< " ++ srcName ++ ", Tail... >"
  [resultType] <- nodeParams_ src
  tellsLn "template< typename... Tail >"
  tellsLn ("auto end_transition( std::shared_ptr< " ++ srcType ++ " > const& src ) {")
  tellsLn "  return src->head.arg1;"
  tellsLn "}"

-------------------------------------------------------------------------------

tellInitialState :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                 => m ()
tellInitialState = do
  automaton <- automaton_
  startName <- pascalCase <$> nodeName_ (lrAutomatonStart automaton)
  tellsLn $ "export function begin(): Fluent<[" ++ startName ++ "]> {"
  tellsLn $ "\treturn new FluentImpl() as any"
  tellsLn "}"

-------------------------------------------------------------------------------

utilScripts = "type Length<T extends unknown[]> = T['length']\n\
\type Prepend<Elm, T extends unknown[]> = ((\n\
\\targ: Elm,\n\
\\t...rest: T\n\
\) => void) extends ((...args: infer T2) => void)\n\
\\t? T2\n\
\\t: never\n\
\\n\
\type Rest<T extends unknown[]> = ((\n\
\\t...rest: T\n\
\) => void) extends ((head: unknown, ...args: infer T2) => void)\n\
\\t? T2\n\
\\t: never\n\
\declare const None: unique symbol\n\
\type None = typeof None\n\
\type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]\n\
\type AddUnknownRest<Tuple extends unknown[], Result extends unknown[] = [...unknown[]]> = {\n\
\\tempty: Result,\n\
\\tnonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)\n\
\\t\t? Prepend<First, AddUnknownRest<Rest<Tuple>, Result>>\n\
\\t\t: never\n\
\}[\n\
\\tTuple extends [unknown, ...unknown[]]\n\
\\t\t? 'nonEmpty'\n\
\\t\t: 'empty'\n\
\]\n\
\\n\
\type CompareLength<Left extends any[], Right extends any[]> = {\n\
\\tfitBoth: 'equal'\n\
\\tfitLeft: 'shorterLeft'\n\
\\tfitRight: 'shorterRight'\n\
\\tunfit: ((..._: Left) => any) extends ((_: any, ..._1: infer LeftRest) => any) ?\n\
\\t\t ((..._: Right) => any) extends ((_: any, ..._1: infer RightRest) => any) ?\n\
\\t\t\t\t\tCompareLength<LeftRest, RightRest>\n\
\\t\t\t: never\n\
\\t\t\t: never\n\
\}[\n\
\\tLeft['length'] extends Right['length'] ? 'fitBoth' :\n\
\\tLeft extends [] ? 'fitLeft' :\n\
\\tRight extends [] ? 'fitRight' :\n\
\\t'unfit'\n\
\]\n\
\\n\
\type StartsWith<Tuple extends unknown[], Tuple2 extends unknown[]> = {\n\
\\tfalse: 0,\n\
\\tempty: 1,\n\
\\tnonEmpty: Head<Tuple> extends Head<Tuple2>\n\
\\t\t? StartsWith<Rest<Tuple>, Rest<Tuple2>>\n\
\\t\t: 0\n\
\}[\n\
\\tCompareLength<Tuple, Tuple2> extends 'shorterLeft'\n\
\\t\t? 'false'\n\
\\t\t: IsFinite<Tuple2, 'finite', 'infinite'> extends 'infinite'\n\
\\t\t\t? 'false'\n\
\\t\t\t: Tuple2 extends [unknown, ...unknown[]]\n\
\\t\t\t\t? 'nonEmpty'\n\
\\t\t\t\t: 'empty'\n\
\]\n\
\type IsFinite<Tuple extends unknown[], Finite, Infinite> = {\n\
\\tempty: Finite\n\
\\tnonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Rest) => unknown)\n\
\\t\t? IsFinite<Rest, Finite, Infinite>\n\
\\t\t: never\n\
\\tinfinite: Infinite\n\
\}[\n\
\\tTuple extends [] ? 'empty' :\n\
\\tTuple extends (infer Element)[] ?\n\
\\tElement[] extends Tuple ?\n\
\\t\t'infinite'\n\
\\t: 'nonEmpty'\n\
\\t: never\n\
\]"
