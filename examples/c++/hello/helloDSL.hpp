
#ifndef ___HELLO_DSL__HPP__
#define ___HELLO_DSL__HPP__

#include <memory>
#include <string>
#include <iostream>

namespace helloDSL {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax helloDSL (start) {
//   simpleHello : start -> "hello()"
//   helloWithName : start -> "hello()" name
//   nameString : name -> "name(string)"
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

class Name {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Name() noexcept;

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

class HelloWithName : public Start, public std::tuple< std::shared_ptr< name > > {
public:
  explicit HelloWithName( std::shared_ptr< name > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NameString : public Name, public std::tuple< string > {
public:
  explicit NameString( string const& arg1 );

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

class Name::Visitor{
public:
  virtual void visitNameString( NameString& host ) = 0;
};

class Name::ConstVisitor{
public:
  virtual void visitNameString( NameString const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Start const& self );
std::ostream& operator <<( std::ostream& out, Name const& self );
std::ostream& operator <<( std::ostream& out, SimpleHello const& self );
std::ostream& operator <<( std::ostream& out, HelloWithName const& self );
std::ostream& operator <<( std::ostream& out, NameString const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
  explicit Node1();
};

class Node2 {
public:
  std::shared_ptr< start > arg1;
  explicit Node2( std::shared_ptr< start > const& arg1_ );
};

class Node3 {
public:
  std::shared_ptr< name > arg1;
  explicit Node3( std::shared_ptr< name > const& arg1_ );
};

class Node4 {
public:
  explicit Node4();
};

class Node5 {
public:
  string arg1;
  explicit Node5( string const& arg1_ );
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
  auto name( string const& arg1 );
  auto hello();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto name_transition( std::shared_ptr< State< Stack... > > const& src, string const& arg1 );

template< typename... Stack >
auto hello_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "helloDSL.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

