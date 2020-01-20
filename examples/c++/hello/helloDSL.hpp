
#ifndef ___HELLO_DSL__HPP__
#define ___HELLO_DSL__HPP__

#include <memory>
#include <string>
#include <iostream>

namespace helloDSL {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax helloDSL (Start) {
//   SimpleHello : Start -> "hello()"
//   HelloWithName : Start -> "hello()" "name(std::string)"
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class Start {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Start() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class SimpleHello : public Start, public std::tuple<  > {
public:
  explicit SimpleHello();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class HelloWithName : public Start, public std::tuple< std::string > {
public:
  explicit HelloWithName( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class Start::Visitor{
public:
  virtual void visitSimpleHello( SimpleHello& host ) = 0;
  virtual void visitHelloWithName( HelloWithName& host ) = 0;
};

class Start::ConstVisitor{
public:
  virtual void visitSimpleHello( SimpleHello const& host ) = 0;
  virtual void visitHelloWithName( HelloWithName const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Start const& self );
std::ostream& operator <<( std::ostream& out, SimpleHello const& self );
std::ostream& operator <<( std::ostream& out, HelloWithName const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
  explicit Node1();
};

class Node2 {
public:
  std::shared_ptr< Start > arg1;
  explicit Node2( std::shared_ptr< Start > const& arg1_ );
};

class Node3 {
public:
  std::string arg1;
  explicit Node3( std::string const& arg1_ );
};

class Node4 {
public:
  explicit Node4();
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
  auto hello();
  auto name( std::string const& arg1 );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto hello_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto name_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "helloDSL.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

