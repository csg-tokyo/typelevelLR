
#ifndef __EXAM2_HPP__
#define __EXAM2_HPP__

#include <memory>
#include <string>
#include <iostream>

namespace exam2 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax exam2 (Graph) {
//   Directed : Graph -> "digraph(std::string)" Stmts
//   Undirected : Graph -> "graph(std::string)" Stmts
//   AndsCons : Ands -> Ands "and_(std::string)"
//   AndsNull : Ands -> eps
//   EdgeAttrColor : EdgeAttr -> "color(std::string)"
//   EdgeAttrStyle : EdgeAttr -> "style(std::string)"
//   EdgeAttrsCons : EdgeAttrs -> EdgeAttrs EdgeAttr
//   EdgeAttrsNull : EdgeAttrs -> eps
//   NodeAttrColor : NodeAttr -> "color(std::string)"
//   NodeAttrShape : NodeAttr -> "shape(std::string)"
//   NodeAttrsCons : NodeAttrs -> NodeAttrs NodeAttr
//   NodeAttrsNull : NodeAttrs -> eps
//   NodeStmt : Stmt -> "node(std::string)" Ands NodeAttrs
//   EdgeStmt : Stmt -> "edge(std::string)" Ands "to(std::string)" Ands EdgeAttrs
//   StmtsCons : Stmts -> Stmts Stmt
//   StmtsNull : Stmts -> eps
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class Graph {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Graph() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Ands {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Ands() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class EdgeAttr {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~EdgeAttr() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class EdgeAttrs {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~EdgeAttrs() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class NodeAttr {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~NodeAttr() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class NodeAttrs {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~NodeAttrs() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Stmt {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Stmt() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Stmts {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Stmts() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class Directed : public Graph, public std::tuple< std::string, std::shared_ptr< Stmts > > {
public:
  explicit Directed( std::string const& arg1, std::shared_ptr< Stmts > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Undirected : public Graph, public std::tuple< std::string, std::shared_ptr< Stmts > > {
public:
  explicit Undirected( std::string const& arg1, std::shared_ptr< Stmts > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class AndsCons : public Ands, public std::tuple< std::shared_ptr< Ands >, std::string > {
public:
  explicit AndsCons( std::shared_ptr< Ands > const& arg1, std::string const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class AndsNull : public Ands, public std::tuple<  > {
public:
  explicit AndsNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class EdgeAttrColor : public EdgeAttr, public std::tuple< std::string > {
public:
  explicit EdgeAttrColor( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class EdgeAttrStyle : public EdgeAttr, public std::tuple< std::string > {
public:
  explicit EdgeAttrStyle( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class EdgeAttrsCons : public EdgeAttrs, public std::tuple< std::shared_ptr< EdgeAttrs >, std::shared_ptr< EdgeAttr > > {
public:
  explicit EdgeAttrsCons( std::shared_ptr< EdgeAttrs > const& arg1, std::shared_ptr< EdgeAttr > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class EdgeAttrsNull : public EdgeAttrs, public std::tuple<  > {
public:
  explicit EdgeAttrsNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NodeAttrColor : public NodeAttr, public std::tuple< std::string > {
public:
  explicit NodeAttrColor( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NodeAttrShape : public NodeAttr, public std::tuple< std::string > {
public:
  explicit NodeAttrShape( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NodeAttrsCons : public NodeAttrs, public std::tuple< std::shared_ptr< NodeAttrs >, std::shared_ptr< NodeAttr > > {
public:
  explicit NodeAttrsCons( std::shared_ptr< NodeAttrs > const& arg1, std::shared_ptr< NodeAttr > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NodeAttrsNull : public NodeAttrs, public std::tuple<  > {
public:
  explicit NodeAttrsNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NodeStmt : public Stmt, public std::tuple< std::string, std::shared_ptr< Ands >, std::shared_ptr< NodeAttrs > > {
public:
  explicit NodeStmt( std::string const& arg1, std::shared_ptr< Ands > const& arg2, std::shared_ptr< NodeAttrs > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class EdgeStmt : public Stmt, public std::tuple< std::string, std::shared_ptr< Ands >, std::string, std::shared_ptr< Ands >, std::shared_ptr< EdgeAttrs > > {
public:
  explicit EdgeStmt( std::string const& arg1, std::shared_ptr< Ands > const& arg2, std::string const& arg3, std::shared_ptr< Ands > const& arg4, std::shared_ptr< EdgeAttrs > const& arg5 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class StmtsCons : public Stmts, public std::tuple< std::shared_ptr< Stmts >, std::shared_ptr< Stmt > > {
public:
  explicit StmtsCons( std::shared_ptr< Stmts > const& arg1, std::shared_ptr< Stmt > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class StmtsNull : public Stmts, public std::tuple<  > {
public:
  explicit StmtsNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class Graph::Visitor{
public:
  virtual void visitDirected( Directed& host ) = 0;
  virtual void visitUndirected( Undirected& host ) = 0;
};

class Graph::ConstVisitor{
public:
  virtual void visitDirected( Directed const& host ) = 0;
  virtual void visitUndirected( Undirected const& host ) = 0;
};

class Ands::Visitor{
public:
  virtual void visitAndsCons( AndsCons& host ) = 0;
  virtual void visitAndsNull( AndsNull& host ) = 0;
};

class Ands::ConstVisitor{
public:
  virtual void visitAndsCons( AndsCons const& host ) = 0;
  virtual void visitAndsNull( AndsNull const& host ) = 0;
};

class EdgeAttr::Visitor{
public:
  virtual void visitEdgeAttrColor( EdgeAttrColor& host ) = 0;
  virtual void visitEdgeAttrStyle( EdgeAttrStyle& host ) = 0;
};

class EdgeAttr::ConstVisitor{
public:
  virtual void visitEdgeAttrColor( EdgeAttrColor const& host ) = 0;
  virtual void visitEdgeAttrStyle( EdgeAttrStyle const& host ) = 0;
};

class EdgeAttrs::Visitor{
public:
  virtual void visitEdgeAttrsCons( EdgeAttrsCons& host ) = 0;
  virtual void visitEdgeAttrsNull( EdgeAttrsNull& host ) = 0;
};

class EdgeAttrs::ConstVisitor{
public:
  virtual void visitEdgeAttrsCons( EdgeAttrsCons const& host ) = 0;
  virtual void visitEdgeAttrsNull( EdgeAttrsNull const& host ) = 0;
};

class NodeAttr::Visitor{
public:
  virtual void visitNodeAttrColor( NodeAttrColor& host ) = 0;
  virtual void visitNodeAttrShape( NodeAttrShape& host ) = 0;
};

class NodeAttr::ConstVisitor{
public:
  virtual void visitNodeAttrColor( NodeAttrColor const& host ) = 0;
  virtual void visitNodeAttrShape( NodeAttrShape const& host ) = 0;
};

class NodeAttrs::Visitor{
public:
  virtual void visitNodeAttrsCons( NodeAttrsCons& host ) = 0;
  virtual void visitNodeAttrsNull( NodeAttrsNull& host ) = 0;
};

class NodeAttrs::ConstVisitor{
public:
  virtual void visitNodeAttrsCons( NodeAttrsCons const& host ) = 0;
  virtual void visitNodeAttrsNull( NodeAttrsNull const& host ) = 0;
};

class Stmt::Visitor{
public:
  virtual void visitNodeStmt( NodeStmt& host ) = 0;
  virtual void visitEdgeStmt( EdgeStmt& host ) = 0;
};

class Stmt::ConstVisitor{
public:
  virtual void visitNodeStmt( NodeStmt const& host ) = 0;
  virtual void visitEdgeStmt( EdgeStmt const& host ) = 0;
};

class Stmts::Visitor{
public:
  virtual void visitStmtsCons( StmtsCons& host ) = 0;
  virtual void visitStmtsNull( StmtsNull& host ) = 0;
};

class Stmts::ConstVisitor{
public:
  virtual void visitStmtsCons( StmtsCons const& host ) = 0;
  virtual void visitStmtsNull( StmtsNull const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Graph const& self );
std::ostream& operator <<( std::ostream& out, Ands const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttr const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttrs const& self );
std::ostream& operator <<( std::ostream& out, NodeAttr const& self );
std::ostream& operator <<( std::ostream& out, NodeAttrs const& self );
std::ostream& operator <<( std::ostream& out, Stmt const& self );
std::ostream& operator <<( std::ostream& out, Stmts const& self );
std::ostream& operator <<( std::ostream& out, Directed const& self );
std::ostream& operator <<( std::ostream& out, Undirected const& self );
std::ostream& operator <<( std::ostream& out, AndsCons const& self );
std::ostream& operator <<( std::ostream& out, AndsNull const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttrColor const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttrStyle const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttrsCons const& self );
std::ostream& operator <<( std::ostream& out, EdgeAttrsNull const& self );
std::ostream& operator <<( std::ostream& out, NodeAttrColor const& self );
std::ostream& operator <<( std::ostream& out, NodeAttrShape const& self );
std::ostream& operator <<( std::ostream& out, NodeAttrsCons const& self );
std::ostream& operator <<( std::ostream& out, NodeAttrsNull const& self );
std::ostream& operator <<( std::ostream& out, NodeStmt const& self );
std::ostream& operator <<( std::ostream& out, EdgeStmt const& self );
std::ostream& operator <<( std::ostream& out, StmtsCons const& self );
std::ostream& operator <<( std::ostream& out, StmtsNull const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
  explicit Node1();
};

class Node2 {
public:
  std::shared_ptr< Graph > arg1;
  explicit Node2( std::shared_ptr< Graph > const& arg1_ );
};

class Node3 {
public:
  std::string arg1;
  explicit Node3( std::string const& arg1_ );
};

class Node4 {
public:
  std::string arg1;
  explicit Node4( std::string const& arg1_ );
};

class Node5 {
public:
  std::string arg1;
  explicit Node5( std::string const& arg1_ );
};

class Node6 {
public:
  std::string arg1;
  explicit Node6( std::string const& arg1_ );
};

class Node7 {
public:
  std::shared_ptr< Ands > arg1;
  explicit Node7( std::shared_ptr< Ands > const& arg1_ );
};

class Node8 {
public:
  std::shared_ptr< Ands > arg1;
  explicit Node8( std::shared_ptr< Ands > const& arg1_ );
};

class Node9 {
public:
  std::shared_ptr< Ands > arg1;
  explicit Node9( std::shared_ptr< Ands > const& arg1_ );
};

class Node10 {
public:
  std::shared_ptr< Stmts > arg1;
  explicit Node10( std::shared_ptr< Stmts > const& arg1_ );
};

class Node11 {
public:
  std::string arg1;
  explicit Node11( std::string const& arg1_ );
};

class Node12 {
public:
  std::string arg1;
  explicit Node12( std::string const& arg1_ );
};

class Node13 {
public:
  std::shared_ptr< EdgeAttrs > arg1;
  explicit Node13( std::shared_ptr< EdgeAttrs > const& arg1_ );
};

class Node14 {
public:
  std::string arg1;
  explicit Node14( std::string const& arg1_ );
};

class Node15 {
public:
  std::shared_ptr< EdgeAttr > arg1;
  explicit Node15( std::shared_ptr< EdgeAttr > const& arg1_ );
};

class Node16 {
public:
  std::shared_ptr< Stmts > arg1;
  explicit Node16( std::shared_ptr< Stmts > const& arg1_ );
};

class Node17 {
public:
  std::string arg1;
  explicit Node17( std::string const& arg1_ );
};

class Node18 {
public:
  std::shared_ptr< NodeAttrs > arg1;
  explicit Node18( std::shared_ptr< NodeAttrs > const& arg1_ );
};

class Node19 {
public:
  std::string arg1;
  explicit Node19( std::string const& arg1_ );
};

class Node20 {
public:
  std::shared_ptr< NodeAttr > arg1;
  explicit Node20( std::shared_ptr< NodeAttr > const& arg1_ );
};

class Node21 {
public:
  std::shared_ptr< Stmt > arg1;
  explicit Node21( std::shared_ptr< Stmt > const& arg1_ );
};

class Node22 {
public:
  std::string arg1;
  explicit Node22( std::string const& arg1_ );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// parsing state

template< typename... Stack >
class State;

template<>
class State<> {};

template< typename Head, typename... Tail >
class State< Head, Tail... > {
public:
  std::weak_ptr< State< Head, Tail... > > this_;
  Head head;
  std::shared_ptr< State< Tail... > > tail;

private:
  State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ );

public:
  static std::shared_ptr< State< Head, Tail... > > make( Head const& head, std::shared_ptr< State< Tail... > > const& tail );

public:
  auto end();
  auto and_( std::string const& arg1 );
  auto color( std::string const& arg1 );
  auto style( std::string const& arg1 );
  auto digraph( std::string const& arg1 );
  auto graph( std::string const& arg1 );
  auto shape( std::string const& arg1 );
  auto node( std::string const& arg1 );
  auto edge( std::string const& arg1 );
  auto to( std::string const& arg1 );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto and__transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto color_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto style_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto digraph_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto graph_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto shape_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto node_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto edge_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto to_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "exam2.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

