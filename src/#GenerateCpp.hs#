
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateCpp where

import Utility
import Syntax
import LALRAutomaton
import CodeGenerateEnv

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo())
import Control.Monad         (forM_, replicateM_)
import Control.Arrow         ((***))
import Control.Monad.Writer  (MonadWriter())
import Control.Monad.Reader  (MonadReader())
import Control.Monad.State

import Data.Either           (isRight)
import Data.Maybe            (mapMaybe)
import Data.List             (isPrefixOf, groupBy)

-------------------------------------------------------------------------------

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellNewline >> tellsLn (replicate 119 '/') >> tellNewline

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
    tellsLn ("  virtual ~" ++ className ++ "() noexcept {}")
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
                  (i, typ) <- zip [1 ..] (ruleParams rule)]
      tells ("class " ++ className ++ " : public " ++ baseClassName)
      tells ", public std::tuple< "
      forMWithSep_ (tells ", ") args $ \(argType, _) -> tells argType
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
    case nodeType of
      NonTerminalSymbol nt -> do
        tellsLn ("  " ++ nonTerminalName nt ++ " content;")
        tellsLn ("  explicit " ++ className ++ "( " ++ nonTerminalName nt ++ " const& content_ );")
      TerminalSymbol (UserTerminal name params) -> do
        forM_ (zip [1 ..] params) $ \(i, param) -> do
          tellsLn ("  " ++ param ++ " arg" ++ show i)
        tells ("  explicit " ++ className)
        case params of
          [] -> tellsLn "();"
          _  ->  do
            tells "( "
            forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
              tells (param ++ " const& arg" ++ show i ++ "_")
            tellsLn " );"
      _ -> return ()
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
  tellsLn "template< typename... Stack >"
  tellsLn "auto end_transition( std::shared_ptr< State< Stack... > > const& src );"
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
  tellNewline
  tellsLn "template< typename... Stack >"
  tellsLn "auto reduce( std::shared_ptr< State< Stack... > > const& src );"

-------------------------------------------------------------------------------

tellBeginPrototype :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                   => m ()
tellBeginPrototype = do
  automaton <- automaton_
  startName <- nodeName_ (lrAutomatonStart automaton)
  tellsLn ("std::shared_ptr< State< " ++ startName ++ " > > begin();")

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
                  (i, typ) <- zip [1 ..] (ruleParams rule)]
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
      let args = ruleParams rule
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
    let argType = nodeType
    tells (className ++ "::" ++ className)
    case argType of
      NonTerminalSymbol nt -> do
        tellsLn ("( " ++ nonTerminalName nt ++ " const& content_ ) :content( content_ ) {}")
      TerminalSymbol (UserTerminal name params) | not (null params) -> do
        tells "( "
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells (param ++ " const& arg" ++ show i ++ "_")
        tells " ) "
        forMWithSep_ (tells ", ") (zip [1 ..] params) $ \(i, param) -> do
          tells (":arg" ++ show i ++ "( arg" ++ show i ++ "_ )")
        tellsLn "{}"
      _ -> tellsLn "() {}"

-------------------------------------------------------------------------------

tellBeginImpl :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
              => m ()
tellBeginImpl = do
  automaton <- automaton_
  startName <- nodeName_ (lrAutomatonStart automaton)
  tellsLn ("std::shared_ptr< State< " ++ startName ++ " > > begin() {")
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
    [tellParsingStateMethodImpls,
     tellTransitionDefaultImpls,
     tellShiftTransitions,
     tellReduceImpls]
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
  tellsLn "  return end_transition( this_ );"
  tellsLn "}"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \terminal -> do
    case terminal of
      UserTerminal methodName [] -> do
        tellsLn "template< typename Head, typename... Tail >"
        tellsLn ("auto State< Head, Tail... >::" ++ methodName ++ "() {")
        tellsLn ("  return " ++ methodName ++ "_transition( this_ );")
        tellsLn "}"
      UserTerminal methodName methodParams -> do
        tellsLn "template< typename Head, typename... Tail >"
        tells ("auto State< Head, Tail... >::" ++ methodName ++ "( ")
        forMWithSep_ (tells ", ") (zip [1 ..] methodParams) $ \(i, param) -> do
          tells (param ++ " arg" ++ show i)
        tellsLn " ) {"
        tells ("  return " ++ methodName ++ "_transition( this_")
        forM_ (zip [1 ..] methodParams) $ \(i, _) -> do
          tells ( ", arg" ++ show i)
        tellsLn " );"
        tellsLn "}"
      _ -> error "unexpected EndOfInput"

-------------------------------------------------------------------------------

tellTransitionDefaultImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                           => m ()
tellTransitionDefaultImpls = do
  syntax <- syntax_
  tellsLn "template< typename... Stack >"
  tellsLn "auto end_transition( std::shared_ptr< State< Stack... > > const& src ) {"
  tellsLn "  return reduce( src )->end();"
  tellsLn "}"
  tellNewline
  forMWithSep_ tellNewline (syntaxTerminals syntax) $ \terminal -> do
    case terminal of
      UserTerminal methodName [] -> do
        tellsLn "template< typename... Stack >"
        tellsLn ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< Stack... > > const& src ) {")
        tellsLn ("  return reduce( src )->" ++ methodName ++ "();")
        tellsLn "}"
      UserTerminal methodName methodParams -> do
        tellsLn "template< typename... Stack >"
        tells ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< Stack... > > const& src")
        forM_ (zip [1 ..] methodParams) $ \(i, param) -> do
          tells (", " ++ param ++ " const& arg" ++ show i)
        tellsLn " ) {"
        tells ("  return reduce( src )->" ++ methodName ++ "( ")
        forMWithSep_ (tells ", ") (zip [1 ..] methodParams) $ \(i, _) -> do
          tells ("arg" ++ show i)
        tellsLn " );"
        tellsLn "}"

-------------------------------------------------------------------------------

tellShiftTransitions :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                     => m ()
tellShiftTransitions = do
  automaton <- automaton_
  nodes     <- nodes_
  tellsLn "// shift transitions"
  tellNewline
  -- acceptible states
  let acceptibleNodes = filter acceptible (map (\(node, _, _) -> node) nodes)
  forMWithSep_ tellNewline acceptibleNodes $ \node -> do
    NodeInfo name (NonTerminalSymbol nt) <- nodeInfo_ node
    tellsLn "template< typename Prev >"
    tellsLn ("inline auto end_transition( std::shared_ptr< State< " ++ name ++ ", Prev > > const& src ) {")
    tellsLn "  return src->head.content;"
    tellsLn "}"
  tellNewline

  let shiftEdges = groupBy (equaling fst) (shifts automaton)
  forMWithSep_ (tellNewline >> tellNewline) shiftEdges $ \edges -> do
    forMWithSep_ tellNewline edges $ \(terminal, (src, dst)) -> do
      (srcName, dstName) <- (,) <$> nodeName_ src <*> nodeName_ dst
      case terminal of
        UserTerminal methodName [] -> do
          tellsLn "template< typename... Tail >"
          tellsLn ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< " ++ srcName ++ ", Tail... > > const& src ) {")
          tellsLn ("  return State< " ++ dstName ++ ", " ++ srcName ++ ", Tail... >::make( " ++ dstName ++ "(), src );")
          tellsLn "}"
        UserTerminal methodName methodParams -> do
          tellsLn "template< typename... Tail >"
          tells ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< " ++ srcName ++ ", Tail... > > const& src")
          forM_ (zip [1 ..] methodParams) $ \(i, param) -> do
            tells (", " ++ param ++ " const& arg" ++ show i)
          tellsLn " ) {"
          tells ("  return State< " ++ dstName ++ ", " ++ srcName ++ ", Tail... >::make( " ++ dstName ++ "( ")
          forMWithSep_ (tells ", ") (zip [1 ..] methodParams) $ \(i, _) -> do
            tells ("arg" ++ show i)
          tellsLn " ), src );"
          tellsLn "}"

-------------------------------------------------------------------------------

tellReduceImpls :: (MonadWriter (Endo String) m, MonadReader CodeGenerateEnv m)
                => m ()
tellReduceImpls = do
  syntax <- syntax_
  automaton <- automaton_
  nodes <- nodes_
  tellsLn "// reduces"
  tellNewline
  forM_ (syntaxNonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \rule -> do
      tells "// " >> tellRule rule >> tellNewline
      forM_ (reduces automaton rule) $ \(src, [base, dst]) -> do
        tellsLn "template< typename... Tail >"
        tells "auto reduce( std::shared_ptr< State< "
        forMWithSep_ (tells ", ") (reverse src) $ \node -> do
          name <- nodeName_ node
          tells name
        tellsLn ", Tail... > > const& src ) {"
        let fresh = modify (+ 1) >> get
        argsCount <- (`execStateT` 0) $ do
          forM_ (zip [0 ..] (tail src)) $ \(i, node) -> do
            typ <- nodeType_ node
            let params = case typ of
                  NonTerminalSymbol nt -> [nonTerminalName nt]
                  TerminalSymbol (UserTerminal _ params) -> params
                  TerminalSymbol EndOfInput              -> []
            forM_ (zip [1 ..] params) $ \(j, param) -> do
              k <- fresh
              tells ("  " ++ param ++ " const& arg" ++ show k ++ " = src")
              replicateM_ i (tells "->tail")
              tellsLn ("->head.arg" ++ show j ++ ";")
        tells ("  " ++ nonTerminalName nt ++ " content( new " ++ pascalCase (ruleName rule) ++ "( ")
        forMWithSep_ (tells ", ") [1 .. argsCount] $ \i -> do
          tells ("arg" ++ show i)
        tellsLn " ) );"
        dstName <- nodeName_ dst
        baseName <- nodeName_ base
        tells ("  std::shared_ptr< State< " ++ baseName ++ ", Tail... > > const& tail = src")
        replicateM_ (length src - 1) (tells "->tail")
        tellsLn ";"
        tellsLn ("  return State< " ++ dstName ++ ", " ++ baseName ++ ", Tail... >::make( " ++ dstName ++ "( content ), tail );")
        tellsLn "}"

-------------------------------------------------------------------------------
