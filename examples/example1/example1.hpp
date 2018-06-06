
#ifndef __EXAMPLE1_HPP__
#define __EXAMPLE1_HPP__

#include <memory>
#include <string>
#include <iostream>

namespace example1 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example1 (A) {
//   genA : A -> "a()" A
//   BToA : A -> B
//   genAB : B -> "a()" B "b()"
//   fin : B -> eps
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class A {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~A() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class B {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~B() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class GenA : public A, public std::tuple< A > {
public:
  explicit GenA( A const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class BToA : public A, public std::tuple< B > {
public:
  explicit BToA( B const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class GenAB : public B, public std::tuple< B > {
public:
  explicit GenAB( B const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Fin : public B, public std::tuple<  > {
public:
  explicit Fin();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class A::Visitor{
public:
  virtual void visitGenA( GenA& host ) = 0;
  virtual void visitBToA( BToA& host ) = 0;
};

class A::ConstVisitor{
public:
  virtual void visitGenA( GenA const& host ) = 0;
  virtual void visitBToA( BToA const& host ) = 0;
};

class B::Visitor{
public:
  virtual void visitGenAB( GenAB& host ) = 0;
  virtual void visitFin( Fin& host ) = 0;
};

class B::ConstVisitor{
public:
  virtual void visitGenAB( GenAB const& host ) = 0;
  virtual void visitFin( Fin const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, A const& self );
std::ostream& operator <<( std::ostream& out, B const& self );
std::ostream& operator <<( std::ostream& out, GenA const& self );
std::ostream& operator <<( std::ostream& out, BToA const& self );
std::ostream& operator <<( std::ostream& out, GenAB const& self );
std::ostream& operator <<( std::ostream& out, Fin const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
};

class Node2 {
public:
  A content;
  explicit Node2( A const& content_ );
};

class Node3 {
public:
  B content;
  explicit Node3( B const& content_ );
};

class Node4 {
public:
  B content;
  explicit Node4( B const& content_ );
};

class Node5 {
public:
  explicit Node5();
};

class Node6 {
public:
  A content;
  explicit Node6( A const& content_ );
};

class Node7 {
public:
  explicit Node7();
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
  auto a();
  auto b();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto a_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto b_transition( std::shared_ptr< State< Stack... > > const& src );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example1.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

