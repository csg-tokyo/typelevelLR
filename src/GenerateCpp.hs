
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateCpp where

import Utility
import Syntax
import LALRAutomaton
import LRTable
import CodeGenerateEnv hiding (symbolParams, ruleParams, nodeParams_)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo(appEndo))
import Control.Monad         (forM_, replicateM_)
import Control.Arrow         ((***))
import Control.Monad.Writer  (MonadWriter(), execWriter)
import Control.Monad.Reader  (MonadReader(), runReaderT)
import Control.Monad.State

import Data.Either           (isRight)
import Data.Maybe            (mapMaybe)
import Data.List             (isPrefixOf, groupBy, intercalate)

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellNewline >> tellsLn (replicate 119 '/') >> tellNewline

-------------------------------------------------------------------------------

sharedPtr :: String -> String
sharedPtr typ = "std::shared_ptr< " ++ typ ++ " >"

weakPtr :: String -> String
weakPtr typ = "std::weak_ptr< " ++ typ ++ " >"

-------------------------------------------------------------------------------

nonTerminalType :: NonTerminal -> String
nonTerminalType nt = sharedPtr (nonTerminalName nt)

symbolParams :: Symbol -> [String]
symbolParams (NonTerminalSymbol nt) = [nonTerminalType nt]
symbolParams (TerminalSymbol (UserTerminal t params)) = params
symbolParams (TerminalSymbol EndOfInput) = []

ruleParams :: Rule -> [[String]]
ruleParams rule = map symbolParams (ruleRhs rule)

nodeParams_ :: (MonadReader CodeGenerateEnv m) => LRNode -> m [String]
nodeParams_ node = symbolParams <$> nodeType_ node

-------------------------------------------------------------------------------

generate :: Syntax -> IO ()
generate s = do
  let name = syntaxName s
  let env = buildCodeGenerateEnv s (lalrAutomaton s)
  writeFile (name ++ ".hpp") (appEndo (execWriter (runReaderT tellHpp env)) "")
  writeFile (name ++ ".cpp") (appEndo (execWriter (runReaderT tellCpp env)) "")
  writeFile (name ++ ".hpp.impl") (appEndo (execWriter (runReaderT tellHppImpl env)) "")

-------------------------------------------------------------------------------
-- generate .hpp
-------------------------------------------------------------------------------

tellHpp :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
        => m ()
tellHpp = do
  moduleName <- syntaxName <$> syntax_
  tellNewline
  tellsLn ("#ifndef __" ++ allCaps moduleName ++ "_HPP__")
  tellsLn ("#define __" ++ allCaps moduleName ++ "_HPP__")
  tellNewline
  tellIncludes
  tellNewline
  tellsLn ("namespace " ++ moduleName ++ " {")
  tellSeparator
  tellGrammar
  tellSeparator
  sequenceWithSep_ tellSeparator $
    [tellASTBasePrototypes,
     tellASTSubPrototypes,
     tellVisitorPrototypes,
     tellASTPrintPrototypes,
     tellAutomatonNodePrototypes,
     tellParsingStatePrototype,
     tellTransitionPrototypes,
     tellBeginPrototype]
  tellSeparator
  let hppImplName = moduleName ++ ".hpp.impl"
  tellsLn ("#include \"" ++ hppImplName ++ "\"")
  tellSeparator
  tellsLn "}"
  tellNewline
  tellsLn "#endif"
  tellNewline

-------------------------------------------------------------------------------

tellIncludes :: (MonadWriter (Endo String) m) => m ()
tellIncludes = do
  tellsLn "#include <memory>"
  tellsLn "#include <string>"
  tellsLn "#include <iostream>"

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
            => m ()
tellGrammar = do
  syntax <- syntax_
  tellsLn "// grammar definition"
  tellNewline
  tellsLn ("// syntax " ++ syntaxName syntax ++ " (" ++ nonTerminalName (syntaxStart syntax) ++ ") {")
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "//   " >> tellRule rule >> tellNewline
  tellsLn "// }"

-------------------------------------------------------------------------------

tellASTBasePrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                      => m ()
tellASTBasePrototypes = do
  syntax <- syntax_
  tellsLn "// AST node abstract classes"
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("class " ++ className ++ " {")
    tellsLn "public:"
    tellsLn "  class Visitor;"
    tellsLn "  class ConstVisitor;"
    tellNewline
    tellsLn ("  virtual ~" ++ className ++ "() noexcept;")
    tellNewline
    tellsLn "  virtual void accept( Visitor& ) = 0;"
    tellsLn "  virtual void accept( ConstVisitor& ) const = 0;"
    tellsLn "};"

-------------------------------------------------------------------------------

tellASTSubPrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => m ()
tellASTSubPrototypes = do
  syntax <- syntax_
  tellsLn "// AST node concrete classes"
  tellNewline
  forMWithSep_ (tellNewline >> tellNewline) (syntaxNonTerminals syntax) $ \nt -> do
    let baseClassName = pascalCase (nonTerminalName nt)
    forMWithSep_ tellNewline (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (concat (ruleParams rule))]
      tells ("class " ++ className ++ " : public " ++ baseClassName)
      tells ", public std::tuple< "
      forMWithSep_ (tells ", ") args $ \(argType, argName) -> tells argType
      tellsLn " > {"
      tellsLn "public:"
      -- constructor
      if null args
        then tellsLn ("  explicit " ++ className ++ "();")
        else do
          tells ("  explicit " ++ className ++ "( ")
          forMWithSep_ (tells ", ") args $ \(typ, name) -> do
            tells (typ ++ " const& " ++ name)
          tellsLn " );"
      tellNewline
      -- void accept( Visitor& )
      tellsLn "  void accept( Visitor& visitor );"
      tellsLn "  void accept( ConstVisitor& visitor ) const;"
      tellsLn "};"

-------------------------------------------------------------------------------

tellVisitorPrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                      => m ()
tellVisitorPrototypes = do
  syntax <- syntax_
  tellsLn "// visitors"
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    let ownerClassName = pascalCase (nonTerminalName nt)
    forMWithSep_ tellNewline [("", "Visitor"), (" const", "ConstVisitor")] $ \(const, visitorType) -> do
      tellsLn ("class " ++ ownerClassName ++ "::" ++ visitorType ++ "{")
      tellsLn "public:"
      forM_ (syntaxRules syntax nt) $ \rule -> do
        let hostClassName = pascalCase (ruleName rule)
        tells ("  virtual void visit" ++ hostClassName)
        tellsLn ("( " ++ hostClassName ++ const ++ "& host ) = 0;")
      tellsLn "};"

-------------------------------------------------------------------------------

tellASTPrintPrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                       => m ()
tellASTPrintPrototypes = do
  syntax <- syntax_
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("std::ostream& operator <<( std::ostream& out, " ++ className ++ " const& self );")
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      tellsLn ("std::ostream& operator <<( std::ostream& out, " ++ className ++ " const& self );")

-------------------------------------------------------------------------------

tellAutomatonNodePrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                            => m ()
tellAutomatonNodePrototypes = do
  nodes <- nodes_
  tellsLn "// automaton nodes"
  tellNewline
  forMWithSep_ tellNewline nodes $ \(node, nodeName, nodeType) -> do
    let className = pascalCase nodeName
    tellsLn ("class " ++ className ++ " {")
    tellsLn "public:"
    params <- nodeParams_ node
    case params of
      [] -> tellsLn ("  explicit " ++ className ++ "();")
      _  -> do
        forM_ (zip [1 ..] params) $ \(i, param) -> do
          tellsLn ("  " ++ param ++ " arg" ++ show i ++ ";")
        tells ("  explicit " ++ className ++ "( ")
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells (param ++ " const& arg" ++ show i ++ "_")
        tellsLn " );"
    tellsLn "};"

-------------------------------------------------------------------------------

tellParsingStatePrototype :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                          => m ()
tellParsingStatePrototype = do
  syntax <- syntax_
  tellsLn "// parsing state"
  tellNewline
  tellsLn "template< typename... Stack >"
  tellsLn "class State;"
  tellNewline
  tellsLn "template<>"
  tellsLn "class State<> {};"
  tellNewline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "class State< Head, Tail... > {"
  tellsLn "public:"
  tellsLn "  std::weak_ptr< State< Head, Tail... > > this_;"
  tellsLn "  Head head;"
  tellsLn "  std::shared_ptr< State< Tail... > > tail;"
  tellNewline
  tellsLn "private:"
  tellsLn "  State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ );"
  tellNewline
  tellsLn "public:"
  tellsLn "  static std::shared_ptr< State< Head, Tail... > > make( Head const& head, std::shared_ptr< State< Tail... > > const& tail );"
  tellNewline
  tellsLn "public:"
  tellsLn "  auto end();"
  forM_ (syntaxTerminals syntax) $ \case
    UserTerminal name params -> do
      tells ("  auto " ++ name)
      case params of
        [] -> tellsLn "();"
        _  -> do
          tells "( "
          forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
            tells (param ++ " const& arg" ++ show i)
          tellsLn " );"
    t             -> error ("invalid terminal symbol found -- " ++ show t)
  tellsLn "};"

-------------------------------------------------------------------------------

tellTransitionPrototypes :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                         => m ()
tellTransitionPrototypes = do
  syntax <- syntax_
  tellsLn "// transition rules"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \t -> do
    case t of
      UserTerminal name params -> do
        tellsLn "template< typename... Stack >"
        tells ("auto " ++ name ++ "_transition( std::shared_ptr< State< Stack... > > const& src")
        forM_ (zip [1 ..] params) $ \(i, param) -> do
          tells (", " ++ param ++ " const& arg" ++ show i)
        tellsLn " );"
      _ -> error ("invalid terminal symbol found -- " ++ show t)
  tellNewline
  tellsLn "template< typename... Stack >"
  tellsLn "auto end_transition( std::shared_ptr< State< Stack... > > const& src );"

-------------------------------------------------------------------------------

tellBeginPrototype :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                   => m ()
tellBeginPrototype = do
  automaton <- automaton_
  startName <- nodeName_ (lrAutomatonStart automaton)
  tellsLn (sharedPtr ("State< " ++ startName ++ " >") ++ " begin();")

-------------------------------------------------------------------------------
-- generate .cpp
-------------------------------------------------------------------------------

tellCpp :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
        => m ()
tellCpp = do
  moduleName <- syntaxName <$> syntax_
  tellNewline
  tellsLn ("#include \"" ++ moduleName ++ ".hpp\"")
  tellNewline
  tellsLn ("namespace " ++ moduleName ++ " {")
  tellSeparator
  sequenceWithSep_ tellSeparator $
    [tellASTMethodImpls,
     tellASTPrintImpls,
     tellAutomatonNodeMethodImpls,
     tellBeginImpl]
  tellSeparator
  tellsLn "}"
  tellNewline

-------------------------------------------------------------------------------

tellASTMethodImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                   => m ()
tellASTMethodImpls = do
  syntax <- syntax_
  -- AST abstract classes
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn (className ++ "::~" ++ className ++ "() noexcept {}")
  tellNewline
  tellNewline
  -- AST concrete classes
  forMWithSep_ (tellNewline >> tellNewline) (syntaxNonTerminals syntax) $ \nt -> do
    forMWithSep_ (tellNewline >> tellNewline) (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (concat (ruleParams rule))]
      -- constructor
      tells (className ++ "::" ++ className ++ "( ")
      forMWithSep_ (tells ", ") args $ \(argType, argName) -> do
        tells (argType ++ " const& " ++ argName)
      tellsLn " )"
      tells "  :std::tuple< "
      forMWithSep_ (tells ", ") args $ \(argType, _) -> do
        tells argType
      tells " >( "
      forMWithSep_ (tells ", ") args $ \(_, argName) -> do
        tells argName
      tellsLn " ) {}"
      tellNewline
      -- void accept( Visitor& )
      forM_ [("", "Visitor"), (" const", "ConstVisitor")] $ \(const, visitorType) -> do
        tellsLn ("void " ++ className ++ "::accept( " ++ visitorType ++ "& visitor )" ++ const ++ " {")
        tellsLn ("  visitor.visit" ++ className ++ "( *this );")
        tellsLn "}"

-------------------------------------------------------------------------------

tellASTPrintImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                  => m ()
tellASTPrintImpls = do
  syntax <- syntax_
  -- AST abstract classes
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("std::ostream& operator <<( std::ostream &out, " ++ className ++ " const& self ) {")
    tellsLn ("  class Visitor : public " ++ className ++ "::ConstVisitor {")
    tellsLn "  public:"
    tellsLn "    std::ostream* out_;"
    tellsLn "    Visitor( std::ostream& out ):out_( &out ){}"
    forM_ (syntaxRules syntax nt) $ \rule -> do
      let hostClassName = pascalCase (ruleName rule)
      tells ("    void visit" ++ hostClassName)
      tellsLn ("( " ++ hostClassName ++ " const& host ) {")
      tellsLn "      *out_ << host;"
      tellsLn "    }"
    tellsLn "  } visitor( out );"
    tellsLn "  self.accept( visitor );"
    tellsLn "  return out;"
    tellsLn "}"
  tellNewline
  tellNewline
  forMWithSep_ tellNewline (syntaxNonTerminals syntax) $ \nt -> do
    forMWithSep_ tellNewline (syntaxRules syntax nt) $ \rule -> do
      let className = pascalCase (ruleName rule)
      let args = concat (ruleParams rule)
      tellsLn ("std::ostream& operator <<( std::ostream& out, " ++ className ++ " const& self ) {")
      tells ("  out << \"" ++ className ++ "(\"")
      forMWithSep_ (tells " << \", \"") (zip [0 ..] args) $ \(i, typeName) -> do
        if "std::shared_ptr" `isPrefixOf` typeName
          then tells ("<< *std::get< " ++ show i ++ " >( self )")
          else tells ("<<  std::get< " ++ show i ++ " >( self )")
      tellsLn " << \")\";"
      tellsLn "  return out;"
      tellsLn "}"

-------------------------------------------------------------------------------

tellAutomatonNodeMethodImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                             => m ()
tellAutomatonNodeMethodImpls = do
  nodes <- nodes_
  forMWithSep_ tellNewline nodes $ \(node, nodeName, nodeType) -> do
    let className = pascalCase nodeName
    params <- nodeParams_ node
    case params of
      [] -> tellsLn (className ++ "::" ++ className ++ "() {}")
      _  -> do
        tells (className ++ "::" ++ className ++ "( ")
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells (param ++ " const& arg" ++ show i ++ "_")
        tellsLn ")"
        tells "    :"
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells ("arg" ++ show i ++ "( arg" ++ show i ++ "_ )")
        tellsLn " {}"

-------------------------------------------------------------------------------

tellBeginImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
              => m ()
tellBeginImpl = do
  automaton <- automaton_
  startName <- nodeName_ (lrAutomatonStart automaton)
  tellsLn (sharedPtr ("State< " ++ startName ++ " >") ++ " begin() {")
  tellsLn "  std::shared_ptr< State<> > bottom( new State<>() );"
  tellsLn ("  return State< " ++ startName ++ " >::make( " ++ startName ++ "(), bottom );")
  tellsLn "}"

-------------------------------------------------------------------------------
-- generate .hpp.impl
-------------------------------------------------------------------------------

tellHppImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
            => m ()
tellHppImpl = do
  tellSeparator
  sequenceWithSep_ tellSeparator $
    -- [tellParsingStateMethodImpls,
    --  tellTransitionDefaultImpls,
    --  tellShiftTransitions,
    --  tellReduceImpls]
    [tellParsingStateMethodImpls,
     tellTransitions]
  tellSeparator

-------------------------------------------------------------------------------

tellParsingStateMethodImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                            => m ()
tellParsingStateMethodImpls = do
  syntax <- syntax_
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "State< Head, Tail... >::State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ )"
  tellsLn "  :head( head_ ), tail( tail_ ) {}"
  tellNewline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "std::shared_ptr< State< Head, Tail... > > State< Head, Tail... >::make( Head const& head, std::shared_ptr< State< Tail... > > const& tail ) {"
  tellsLn "  std::shared_ptr< State< Head, Tail... > > result( new State< Head, Tail... >( head, tail ) );"
  tellsLn "  result->this_ = result;"
  tellsLn "  return result;"
  tellsLn "}"
  tellNewline
  tellNewline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "auto State< Head, Tail... >::end() {"
  tellsLn "  return end_transition( this_.lock() );"
  tellsLn "}"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \terminal -> do
    case terminal of
      UserTerminal methodName [] -> do
        tellsLn "template< typename Head, typename... Tail >"
        tellsLn ("auto State< Head, Tail... >::" ++ methodName ++ "() {")
        tellsLn ("  return " ++ methodName ++ "_transition( this_.lock() );")
        tellsLn "}"
      UserTerminal methodName methodParams -> do
        tellsLn "template< typename Head, typename... Tail >"
        tells ("auto State< Head, Tail... >::" ++ methodName ++ "( ")
        forMWithSep_ (tells ", ") (zip [1 ..] methodParams) $ \(i, param) -> do
          tells (param ++ " const& arg" ++ show i)
        tellsLn " ) {"
        tells ("  return " ++ methodName ++ "_transition( this_.lock()")
        forM_ (zip [1 ..] methodParams) $ \(i, _) -> do
          tells ( ", arg" ++ show i)
        tellsLn " );"
        tellsLn "}"
      _ -> error "unexpected EndOfInput"

-------------------------------------------------------------------------------

tellTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                => m ()
tellTransitions = do
  table <- lrTable_
  tellsLn "// transitions"
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
  let funName = terminalName t ++ "_transition"
  let srcType = "State< " ++ srcName ++ ", Tail... >"
  let dstType = "State< " ++ dstName ++ ", " ++ srcName ++ ", Tail... >"
  let params = terminalParams t
  let paramList = sharedPtr srcType ++ " const& src" ++ concat [", " ++ typ ++ " const& arg" ++ show i | (i, typ) <- zip [1 ..] params]
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
    let paramList = sharedPtr srcType ++ " const& src" ++ concat [", " ++ typ ++ " const& arg" ++ show i | (i, typ) <- zip [1 ..] params]
    dstName <- pascalCase <$> nodeName_ (head dstPath)
    let contentType = sharedPtr (nonTerminalName (ruleLhs rule))
    tellsLn "template< typename... Tail >"
    tellsLn ("auto " ++ funName ++ "( " ++ paramList ++ " ) {")
    n <- (`execStateT` 0) $ forM_ (zip [1 ..] (ruleRhs rule)) $ \(i, sym) -> case sym of
      NonTerminalSymbol nt -> do
        j <- modify (+ 1) >> get
        tellsLn ("  " ++ sharedPtr (nonTerminalName nt) ++ " const& x" ++ show j ++ " = src->" ++ ([1 .. length (ruleRhs rule) - i] *> "tail->") ++ "head.arg1;")
      TerminalSymbol t -> do
        forM_ (zip [1 ..] (terminalParams t)) $ \(k, param) -> do
          j <- modify (+ 1) >> get
          tellsLn ("  " ++ param ++ " const& x" ++ show j ++ " = src->" ++ ([1 .. length (ruleRhs rule) - i] *> "tail->") ++ "head.arg" ++ show k ++ ";")
    tellsLn ("  " ++ contentType ++ " const& content = " ++ contentType ++ "( new " ++ ruleName rule ++ "( " ++ intercalate ", " ["x" ++ show i | i <- [1 .. n]] ++ " ) );")
    tellsLn ("  " ++ sharedPtr baseType ++ " const& tail = src" ++ concat ["->tail" | _ <- tail srcPath] ++ ";")
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
