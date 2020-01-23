
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
       tellTerminalMethodDefinitions,
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
    tellsLn "\tabstract accept(v? : Visitor): void"
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
          tellsLn "\t) {"
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

-------------------------------------------------------------------------------

tellTerminalMethodDefinitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m) =>
                                 m ()
tellTerminalMethodDefinitions = do
  syntax <- syntax_
  tellsLn "// terminal symbols"
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
  tellsLn $ "export function begin(): Fluent<[" ++ startName ++ "]> {"
  tellsLn $ "\treturn new FluentImpl() as any"
  tellsLn "}"

-------------------------------------------------------------------------------

utilScripts = "type Length<T extends unknown[]> = T['length']\n\
\type Prepend<Elm, T extends unknown[]> = ((\n\
\    arg: Elm,\n\
\    ...rest: T\n\
\) => void) extends ((...args: infer T2) => void)\n\
\    ? T2\n\
\    : never\n\
\\n\
\type Rest<T extends unknown[]> = ((\n\
\    ...rest: T\n\
\) => void) extends ((head: unknown, ...args: infer T2) => void)\n\
\    ? T2\n\
\    : never\n\
\declare const None: unique symbol\n\
\type None = typeof None\n\
\type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]\n\
\type AddUnknownRest<Tuple extends unknown[], Result extends unknown[] = [...unknown[]]> = {\n\
\    empty: Result,\n\
\    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)\n\
\      ? Prepend<First, AddUnknownRest<Rest<Tuple>, Result>>\n\
\      : never\n\
\}[\n\
\    Tuple extends [unknown, ...unknown[]]\n\
\      ? 'nonEmpty'\n\
\      : 'empty'\n\
\]\n\
\type CompareLength<Left extends any[], Right extends any[]> = {\n\
\    fitBoth: 'equal'\n\
\    fitLeft: 'shorterLeft'\n\
\    fitRight: 'shorterRight'\n\
\    unfit: ((..._: Left) => any) extends ((_: any, ..._1: infer LeftRest) => any) ?\n\
\         ((..._: Right) => any) extends ((_: any, ..._1: infer RightRest) => any) ?\n\
\            CompareLength<LeftRest, RightRest>\n\
\        : never\n\
\        : never\n\
\}[\n\
\    Left['length'] extends Right['length'] ? 'fitBoth' :\n\
\    Left extends [] ? 'fitLeft' :\n\
\    Right extends [] ? 'fitRight' :\n\
\    'unfit'\n\
\]\n\
\type StartsWith<Tuple extends unknown[], Tuple2 extends unknown[]> = {\n\
\    false: 0,\n\
\    empty: 1,\n\
\    nonEmpty: Head<Tuple> extends Head<Tuple2>\n\
\        ? StartsWith<Rest<Tuple>, Rest<Tuple2>>\n\
\        : 0\n\
\}[\n\
\    CompareLength<Tuple, Tuple2> extends 'shorterLeft'\n\
\        ? 'false'\n\
\        : IsFinite<Tuple2, 'finite', 'infinite'> extends 'infinite'\n\
\            ? 'false'\n\
\            : Tuple2 extends [unknown, ...unknown[]]\n\
\                ? 'nonEmpty'\n\
\                : 'empty'\n\
\]\n\
\type IsFinite<Tuple extends unknown[], Finite, Infinite> = {\n\
\    empty: Finite\n\
\    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Rest) => unknown)\n\
\      ? IsFinite<Rest, Finite, Infinite>\n\
\      : never\n\
\    infinite: Infinite\n\
\}[\n\
\    Tuple extends [] ? 'empty' :\n\
\    Tuple extends (infer Element)[] ?\n\
\    Element[] extends Tuple ?\n\
\      'infinite'\n\
\    : 'nonEmpty'\n\
\    : never\n\
\]"
