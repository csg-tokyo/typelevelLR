
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module GenerateCpp where

import Utility
import Syntax
import LALRAutomaton

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid           (Endo())
import Control.Monad         (forM_, replicateM_)
import Control.Arrow         ((***))
import Control.Monad.Writer  (MonadWriter())

import Data.Either           (isRight)
import Data.Maybe            (mapMaybe)
import Data.List             (isPrefixOf)

-------------------------------------------------------------------------------

data NodeInfo = NodeInfo { nodeName :: String,
                           nodeType :: Symbol }
  deriving (Show)

type NodeInfoTable = Map.Map LRNode NodeInfo

buildNodeInfoTable :: LALRAutomaton -> NodeInfoTable
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

symbolToTypeName :: Symbol -> Maybe String
symbolToTypeName (Left nt) = Just ("std::shared_ptr< " ++ typeName ++ " >")
  where typeName = pascalCase (nonTerminalName nt)
symbolToTypeName (Right StringLiteral) = Just "std::string"
symbolToTypeName (Right IntLiteral)    = Just "int"
symbolToTypeName _                     = Nothing

-------------------------------------------------------------------------------

newline :: (MonadWriter (Endo String) m) => m ()
newline = tells "\n"

tellSeparator :: (MonadWriter (Endo String) m) => m ()
tellSeparator = tellsLn (replicate 119 '/')

-------------------------------------------------------------------------------
-- generate .hpp
-------------------------------------------------------------------------------

tellHpp :: (MonadWriter (Endo String) m) =>
           Syntax -> LALRAutomaton -> NodeInfoTable -> m ()
tellHpp syntax automaton nodeInfos = do
  let moduleName = syntaxName syntax
  newline
  tellsLn ("#ifndef __" ++ allCaps moduleName ++ "_HPP__")
  newline
  tellIncludes
  newline
  tellSeparator
  newline
  tellGrammar syntax
  newline
  tellSeparator
  newline
  tellsLn ("namespace " ++ moduleName ++ " {")
  newline
  sequenceWithSep_ (newline >> tellSeparator >> newline)
    [tellASTBasePrototypes       syntax,
     tellASTSubPrototypes        syntax,
     tellVisitorPrototypes       syntax,
     tellASTPrintPrototypes      syntax,
     tellAutomatonNodePrototypes nodeInfos,
     tellParsingStatePrototype   syntax,
     tellTransitionPrototypes    syntax,
     tellBeginPrototype          automaton nodeInfos]
  newline
  tellSeparator
  newline
  let hppImplName = syntaxName syntax ++ ".hpp.impl"
  tellsLn ("#include \"" ++ hppImplName ++ "\"")
  newline
  tellSeparator
  newline
  tellsLn "}"
  newline
  tellsLn "#endif"
  newline

-------------------------------------------------------------------------------

tellIncludes :: (MonadWriter (Endo String) m) => m ()
tellIncludes = do
  tellsLn "#include <memory>"
  tellsLn "#include <string>"
  tellsLn "#include <iostream>"

-------------------------------------------------------------------------------

tellGrammar :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellGrammar syntax = do
  tellsLn "// grammar definition"
  newline
  tellsLn ("// syntax " ++ syntaxName syntax ++ " (" ++ nonTerminalName (syntaxStart syntax) ++ ") {")
  forM_ (nonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \(ruleName, expr) -> do
      tells ("//   " ++ ruleName ++ " : " ++ nonTerminalName nt ++ " -> ")
      tellExpr expr
      newline
  tellsLn "// }"

-------------------------------------------------------------------------------

tellASTBasePrototypes :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTBasePrototypes syntax = do
  tellsLn "// AST node abstract classes"
  newline
  forMWithSep_ newline (nonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("class " ++ className ++ " {")
    tellsLn "public:"
    tellsLn "  class Visitor;"
    tellsLn "  class ConstVisitor;"
    newline
    tellsLn ("  virtual ~" ++ className ++ "() noexcept;")
    newline
    tellsLn "  virtual void accept( Visitor& ) = 0;"
    tellsLn "  virtual void accept( ConstVisitor& ) const = 0;"
    tellsLn "};"

-------------------------------------------------------------------------------

tellASTSubPrototypes :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTSubPrototypes syntax = do
  tellsLn "// AST node concrete classes"
  newline
  forMWithSep_ (newline >> newline) (nonTerminals syntax) $ \nt -> do
    let baseClassName = pascalCase (nonTerminalName nt)
    forMWithSep_ newline (syntaxRules syntax nt) $ \(ruleName, expr) -> do
      let className = pascalCase ruleName
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (mapMaybe symbolToTypeName expr)]
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
      newline
      -- void accept( Visitor& )
      tellsLn "  void accept( Visitor& visitor );"
      tellsLn "  void accept( ConstVisitor& visitor ) const;"
      tellsLn "};"

-------------------------------------------------------------------------------

tellVisitorPrototypes :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellVisitorPrototypes syntax = do
  tellsLn "// visitors"
  newline
  forMWithSep_ newline (nonTerminals syntax) $ \nt -> do
    let ownerClassName = pascalCase (nonTerminalName nt)
    forMWithSep_ newline [("", "Visitor"), (" const", "ConstVisitor")] $ \(const, visitorType) -> do
      tellsLn ("class " ++ ownerClassName ++ "::" ++ visitorType ++ "{")
      tellsLn "public:"
      forM_ (syntaxRules syntax nt) $ \(ruleName, expr) -> do
        let hostClassName = pascalCase ruleName
        tells ("  virtual void visit" ++ hostClassName)
        tellsLn ("( " ++ hostClassName ++ const ++ "& host ) = 0;")
      tellsLn "};"

-------------------------------------------------------------------------------

tellASTPrintPrototypes :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTPrintPrototypes syntax = do
  forM_ (nonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("std::ostream& operator <<( std::ostream& out, " ++ className ++ " const& self );")
  forM_ (nonTerminals syntax) $ \nt -> do
    forM_ (syntaxRules syntax nt) $ \(ruleName, _) -> do
      let className = pascalCase ruleName
      tellsLn ("std::ostream& operator <<( std::ostream& out, " ++ className ++ " const& self );")

-------------------------------------------------------------------------------

tellAutomatonNodePrototypes :: (MonadWriter (Endo String) m) =>
                                NodeInfoTable -> m ()
tellAutomatonNodePrototypes nodeInfos = do
  tellsLn "// automaton nodes"
  newline
  forMWithSep_ newline nodeInfos $ \(NodeInfo nodeName nodeType) -> do
    let className = pascalCase nodeName
    let argType = symbolToTypeName nodeType
    tellsLn ("class " ++ className ++ " {")
    tellsLn "public:"
    case argType of
      Nothing  -> do
        tellsLn ("  explicit " ++ className ++ "();")
      Just arg -> do
        tellsLn ("  " ++ arg ++ " content;")
        tellsLn ("  explicit " ++ className ++ "( " ++ arg ++ " const& content_ );")
    tellsLn "};"

-------------------------------------------------------------------------------

tellParsingStatePrototype :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellParsingStatePrototype syntax = do
  tellsLn "// parsing state"
  newline
  tellsLn "template< typename... Stack >"
  tellsLn "class State;"
  newline
  tellsLn "template<>"
  tellsLn "class State<> {};"
  newline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "class State< Head, Tail... > {"
  tellsLn "public:"
  tellsLn "  std::shared_ptr< State< Head, Tail... > > this_;"
  tellsLn "  Head head;"
  tellsLn "  std::shared_ptr< State< Tail... > > tail;"
  newline
  tellsLn "private:"
  tellsLn "  State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ );"
  newline
  tellsLn "public:"
  tellsLn "static std::shared_ptr< State< Head, Tail... > > make( Head const& head, std::shared_ptr< State< Tail... > > const& tail );"
  newline
  tellsLn "public:"
  tellsLn "  auto end();"
  forM_ (terminals syntax) $ \case
    Keyword key   -> tellsLn ("  auto " ++ key ++ "();")
    StringLiteral -> tellsLn "  auto str( std::string const& value );"
    IntLiteral    -> tellsLn "  auto num( int const& value );"
    t             -> error ("invalid terminal symbol found -- " ++ show t)    
  tellsLn "};"

-------------------------------------------------------------------------------

tellTransitionPrototypes :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellTransitionPrototypes syntax = do
  tellsLn "// transition rules"
  newline
  tellsLn "template< typename... Stack >"
  tellsLn "auto end_transition( std::shared_ptr< State< Stack... > > const& src );"
  newline
  forMWithSep_ newline (terminals syntax) $ \terminal -> do
    let (name, param) = case terminal of
          Keyword key   -> (key, "")
          StringLiteral -> ("str", ", std::string const& value")
          IntLiteral    -> ("num", ", int const& value")
          t             -> error ("invalid terminal symbol found -- " ++ show t)
    tellsLn "template< typename... Stack >"
    tellsLn ("auto " ++ name ++ "_transition( std::shared_ptr< State< Stack... > > const& src" ++ param ++ " );")
  newline
  newline
  tellsLn "template< typename... Stack >"
  tellsLn "auto reduce( std::shared_ptr< State< Stack... > > const& src );"

-------------------------------------------------------------------------------

tellBeginPrototype :: (MonadWriter (Endo String) m) =>
                      LALRAutomaton -> NodeInfoTable -> m ()
tellBeginPrototype automaton nodeInfos = do
  let startName = nodeName (nodeInfos Map.! lalrAutomatonStart automaton)
  tellsLn ("std::shared_ptr< State< " ++ startName ++ " > > begin();")

-------------------------------------------------------------------------------
-- generate .cpp
-------------------------------------------------------------------------------

tellCpp :: (MonadWriter (Endo String) m) =>
           Syntax -> LALRAutomaton -> NodeInfoTable -> m ()
tellCpp syntax automaton nodeInfos = do
  let moduleName = syntaxName syntax
  newline
  tellsLn ("#include \"" ++ moduleName ++ ".hpp\"")
  newline
  tellsLn ("namespace " ++ moduleName ++ " {")
  newline
  tellSeparator
  newline
  sequenceWithSep_ (newline >> tellSeparator >> newline) $
    [tellASTMethodImpls           syntax,
     tellASTPrintImpls            syntax,
     tellAutomatonNodeMethodImpls nodeInfos,
     tellBeginImpl                automaton nodeInfos]
  newline
  tellSeparator
  newline
  tellsLn "}"
  newline

-------------------------------------------------------------------------------

tellASTMethodImpls :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTMethodImpls syntax = do
  -- AST abstract classes
  forM_ (nonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn (className ++ "::~" ++ className ++ "() noexcept {}")
  newline
  newline
  -- AST concrete classes
  forMWithSep_ (newline >> newline) (nonTerminals syntax) $ \nt -> do
    forMWithSep_ (newline >> newline) (syntaxRules syntax nt) $ \(ruleName, expr) -> do
      let className = pascalCase ruleName
      let args = [(typ, "arg" ++ show i) |
                  (i, typ) <- zip [1 ..] (mapMaybe symbolToTypeName expr)]
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
      newline
      -- void accept( Visitor& )
      forM_ [("", "Visitor"), (" const", "ConstVisitor")] $ \(const, visitorType) -> do
        tellsLn ("void " ++ className ++ "::accept( " ++ visitorType ++ "& visitor )" ++ const ++ " {")
        tellsLn ("  visitor.visit" ++ className ++ "( *this );")
        tellsLn "}"

-------------------------------------------------------------------------------

tellASTPrintImpls :: (MonadWriter (Endo String) m) => Syntax -> m ()
tellASTPrintImpls syntax = do
  -- AST abstract classes
  forMWithSep_ newline (nonTerminals syntax) $ \nt -> do
    let className = pascalCase (nonTerminalName nt)
    tellsLn ("std::ostream& operator <<( std::ostream &out, " ++ className ++ " const& self ) {")
    tellsLn ("  class Visitor : public " ++ className ++ "::ConstVisitor {")
    tellsLn "  public:"
    tellsLn "    std::ostream* out_;"
    tellsLn "    Visitor( std::ostream& out ):out_( &out ){}"
    forM_ (syntaxRules syntax nt) $ \(ruleName, _) -> do
      let hostClassName = pascalCase ruleName
      tells ("    void visit" ++ hostClassName)
      tellsLn ("( " ++ hostClassName ++ " const& host ) {")
      tellsLn "      *out_ << host;"
      tellsLn "    }"
    tellsLn "  } visitor( out );"
    tellsLn "  self.accept( visitor );"
    tellsLn "  return out;"
    tellsLn "}"
  newline
  newline
  forMWithSep_ newline (nonTerminals syntax) $ \nt -> do
    forMWithSep_ newline (syntaxRules syntax nt) $ \(ruleName, expr) -> do
      let className = pascalCase ruleName
      let args = mapMaybe symbolToTypeName expr
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

tellAutomatonNodeMethodImpls :: (MonadWriter (Endo String) m) =>
                                NodeInfoTable -> m ()
tellAutomatonNodeMethodImpls nodeInfos = do
  forMWithSep_ newline nodeInfos $ \(NodeInfo nodeName nodeType) -> do
    let className = pascalCase nodeName
    let argType = symbolToTypeName nodeType
    tells (className ++ "::" ++ className)
    case argType of
      Nothing  -> tellsLn "() {}"
      Just typ -> tellsLn ("( " ++ typ ++ " const& content_ ) :content( content_ ) {}")

-------------------------------------------------------------------------------

tellBeginImpl :: (MonadWriter (Endo String) m) =>
                 LALRAutomaton -> NodeInfoTable -> m ()
tellBeginImpl automaton nodeInfo = do
  let startName = nodeName (nodeInfo Map.! lalrAutomatonStart automaton)
  tellsLn ("std::shared_ptr< State< " ++ startName ++ " > > begin() {")
  tellsLn "  std::shared_ptr< State<> > bottom( new State<>() );"
  tellsLn ("  return State< " ++ startName ++ " >::make( " ++ startName ++ "(), bottom );")
  tellsLn "}"

-------------------------------------------------------------------------------
-- generate .hpp.impl
-------------------------------------------------------------------------------

tellHppImpl :: (MonadWriter (Endo String) m) =>
               Syntax -> LALRAutomaton -> NodeInfoTable -> m ()
tellHppImpl syntax automaton nodeInfos = do
  newline
  tellSeparator
  newline
  sequenceWithSep_ (newline >> tellSeparator >> newline) $
    [tellParsingStateMethodImpls syntax,
     tellTransitionDefaultImpls  syntax,
     tellShiftTransitions        automaton nodeInfos,
     tellReduceImpls             automaton nodeInfos]
  newline
  tellSeparator
  newline

-------------------------------------------------------------------------------

tellParsingStateMethodImpls :: (MonadWriter (Endo String) m) =>
                               Syntax -> m ()
tellParsingStateMethodImpls syntax = do
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "State< Head, Tail... >::State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ )"
  tellsLn "  :head( head_ ), tail( tail_ ) {}"
  newline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "std::shared_ptr< State< Head, Tail... > > State< Head, Tail... >::make( Head const& head, std::shared_ptr< State< Tail... > > const& tail ) {"
  tellsLn "  std::shared_ptr< State< Head, Tail... > > result( new State< Head, Tail... >( head, tail ) );"
  tellsLn "  result->this_ = result;"
  tellsLn "  return result;"
  tellsLn "}"
  newline
  newline
  tellsLn "template< typename Head, typename... Tail >"
  tellsLn "auto State< Head, Tail... >::end() {"
  tellsLn "  return end_transition( this_ );"
  tellsLn "}"
  newline
  forMWithSep_ newline (terminals syntax) $ \terminal -> do
    let (methodName, methodParam, transitionArg) = case terminal of
          Keyword key   -> (key, "", "")
          StringLiteral -> ("str", " std::string const& value ", ", value")
          IntLiteral    -> ("num", " int const& value ", ", value")
    tellsLn "template< typename Head, typename... Tail >"
    tellsLn ("auto State< Head, Tail... >::" ++ methodName ++ "(" ++ methodParam ++ ") {")
    tellsLn ("  return " ++ methodName ++ "_transition( this_" ++ transitionArg ++ " );")
    tellsLn "}"

-------------------------------------------------------------------------------

tellTransitionDefaultImpls :: (MonadWriter (Endo String) m) =>
                              Syntax -> m ()
tellTransitionDefaultImpls syntax = do
  tellsLn "template< typename... Stack >"
  tellsLn "auto end_transition( std::shared_ptr< State< Stack... > > const& src ) {"
  tellsLn "  return reduce( src )->end();"
  tellsLn "}"
  newline
  forMWithSep_ newline (terminals syntax) $ \terminal -> do
    let (methodName, methodParam, transitionArg) = case terminal of
          Keyword key   -> (key, "", "")
          StringLiteral -> ("str", ", std::string const& value", ", value")
          IntLiteral    -> ("num", ", int const& value", ", value")
    tellsLn "template< typename... Stack >"
    tellsLn ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< Stack... > > const& src" ++ methodParam ++ " ) {")
    tellsLn ("  return reduce( src )->" ++ methodName ++ "();")
    tellsLn "}"

-------------------------------------------------------------------------------

tellShiftTransitions :: (MonadWriter (Endo String) m) =>
                        LALRAutomaton -> Map.Map LRNode NodeInfo -> m ()
tellShiftTransitions automaton nodeInfo = do
  tellsLn "// shift transitions"
  newline
  -- acceptible states
  let acceptibleNodes = filter (any (\(LRItem nt _ _ rest) -> nt == StartSymbol && null rest)) (Set.toList (lalrAutomatonNodes automaton))
  forMWithSep_ newline acceptibleNodes $ \node -> do
    let startName = nodeName (nodeInfo Map.! lalrAutomatonStart automaton)
    let endName   = nodeName (nodeInfo Map.! node)
    tellsLn "template<>"
    tellsLn ("inline auto end_transition( std::shared_ptr< State< " ++ endName ++ ", " ++ startName ++ " > > const& src ) {")
    tellsLn "  return src->head.content;"
    tellsLn "}"
  newline

  let edges = lalrAutomatonEdges automaton
  let transitions = filter (not . null . snd) $ do
        (src, dsts) <- Map.toList edges
        let dsts' = [(fromRight symbol, dst) |
                     (symbol, dst) <- Map.toList dsts, isRight symbol]
        return (src, dsts')
  forMWithSep_ (newline >> newline) transitions $ \(src, dsts) -> do
    forMWithSep_ newline dsts $ \(symbol, dst) -> do
      let srcName = nodeName (nodeInfo Map.! src)
      let dstName = nodeName (nodeInfo Map.! dst)
      let (methodName, methodParam, methodArg) = case symbol of
            Keyword key   -> (key, "", "")
            StringLiteral -> ("str", ", std::string const& value", " value ")
            IntLiteral    -> ("num", ", int const& value", " value ")
      tellsLn "template< typename... Tail >"
      tellsLn ("auto " ++ methodName ++ "_transition( std::shared_ptr< State< " ++ srcName ++ ", Tail... > > const& src" ++ methodParam ++ " ) {")
      tellsLn ("  return State< " ++ dstName ++ ", " ++ srcName ++ ", Tail... >::make( " ++ dstName ++ "(" ++ methodArg ++ "), src );")
      tellsLn "}"

-------------------------------------------------------------------------------

tellReduceImpls :: (MonadWriter (Endo String) m) =>
                   LALRAutomaton -> NodeInfoTable -> m ()
tellReduceImpls automaton nodeInfo = do
  tellsLn "// reduces"
  newline
  forMWithSep_ newline (reduces automaton) $ \(src, ruleName, dst) -> do
    let NodeInfo dstName dstType = nodeInfo Map.! dst
    let Just dstTypeName = symbolToTypeName dstType
    let baseName = nodeName (nodeInfo Map.! last src)
    tellsLn "template< typename... Tail >"
    tells "auto reduce( std::shared_ptr< State< "
    forMWithSep_ (tells ", ") src $ \node -> do
      tells (nodeName (nodeInfo Map.! node))
    tellsLn ", Tail... > > const& src ) {"
    let getContentType = symbolToTypeName . nodeType . (nodeInfo Map.!)
    let args = mapMaybe (sequence . (id *** getContentType)) (zip [0 ..] (init src))
    forM_ (zip [1 ..] (reverse args)) $ \(i, (n, contentType)) -> do
      tells ("  " ++ contentType ++ " const& arg" ++ show i ++ " = ")
      tells "src"
      replicateM_ n (tells "->tail")
      tellsLn "->head.content;"
    tells ("  " ++ dstTypeName ++ " content( new " ++ pascalCase ruleName ++ "( ")
    forMWithSep_ (tells ", ") [1 .. length args] $ \i -> tells ("arg" ++ show i)
    tellsLn " ) );"
    tells ("  std::shared_ptr< State< " ++ baseName ++ ", Tail... > > const& tail = ")
    tells "src"
    replicateM_ (length src - 1) (tells "->tail")
    tellsLn ";"
    tellsLn ("  return State< " ++ dstName ++ ", " ++ baseName ++ ", Tail... >::make( " ++ dstName ++ "( content ), tail );")
    tellsLn "}"

-------------------------------------------------------------------------------
