
#ifndef __EXAMPLE4_HPP__
#define __EXAMPLE4_HPP__

#include <memory>
#include <string>
#include <iostream>

namespace example4 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example4 (Start) {
//   simpleHello : Start -> "hello()"
//   helloWithName : Start -> "hello()" Name
//   nameString : Name -> str
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class Start {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Start() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Name {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Name() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Str {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Str() noexcept {}

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

class HelloWithName : public Start, public std::tuple< Name > {
public:
  explicit HelloWithName( Name const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NameString : public Name, public std::tuple< Str > {
public:
  explicit NameString( Str const& arg1 );

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

class Str::Visitor{
public:
};

class Str::ConstVisitor{
public:
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Start const& self );
std::ostream& operator <<( std::ostream& out, Name const& self );
std::ostream& operator <<( std::ostream& out, Str const& self );
std::ostream& operator <<( std::ostream& out, SimpleHello const& self );
std::ostream& operator <<( std::ostream& out, HelloWithName const& self );
std::ostream& operator <<( std::ostream& out, NameString const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
};

class Node2 {
public:
  Start content;
  explicit Node2( Start const& content_ );
};

class Node3 {
public:
  Name content;
  explicit Node3( Name const& content_ );
};

class Node4 {
public:
  explicit Node4();
};

class Node5 {
public:
  str content;
  explicit Node5( str const& content_ );
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
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto hello_transition( std::shared_ptr< State< Stack... > > const& src );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example4.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

