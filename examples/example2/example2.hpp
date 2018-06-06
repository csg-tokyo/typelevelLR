
#ifndef __EXAMPLE2_HPP__
#define __EXAMPLE2_HPP__

#include <memory>
#include <string>
#include <iostream>

namespace example2 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example2 (E) {
//   Add : E -> E "add()" T
//   TToE : E -> T
//   Num : F -> "num(Int)"
//   Paren : F -> "lp()" E "rp()"
//   Mul : T -> T "mul()" F
//   FToT : T -> F
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class E {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~E() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class F {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~F() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class T {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~T() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class Add : public E, public std::tuple< E, T > {
public:
  explicit Add( E const& arg1, T const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TToE : public E, public std::tuple< T > {
public:
  explicit TToE( T const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Num : public F, public std::tuple< Int > {
public:
  explicit Num( Int const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Paren : public F, public std::tuple< E > {
public:
  explicit Paren( E const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Mul : public T, public std::tuple< T, F > {
public:
  explicit Mul( T const& arg1, F const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FToT : public T, public std::tuple< F > {
public:
  explicit FToT( F const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class E::Visitor{
public:
  virtual void visitAdd( Add& host ) = 0;
  virtual void visitTToE( TToE& host ) = 0;
};

class E::ConstVisitor{
public:
  virtual void visitAdd( Add const& host ) = 0;
  virtual void visitTToE( TToE const& host ) = 0;
};

class F::Visitor{
public:
  virtual void visitNum( Num& host ) = 0;
  virtual void visitParen( Paren& host ) = 0;
};

class F::ConstVisitor{
public:
  virtual void visitNum( Num const& host ) = 0;
  virtual void visitParen( Paren const& host ) = 0;
};

class T::Visitor{
public:
  virtual void visitMul( Mul& host ) = 0;
  virtual void visitFToT( FToT& host ) = 0;
};

class T::ConstVisitor{
public:
  virtual void visitMul( Mul const& host ) = 0;
  virtual void visitFToT( FToT const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, E const& self );
std::ostream& operator <<( std::ostream& out, F const& self );
std::ostream& operator <<( std::ostream& out, T const& self );
std::ostream& operator <<( std::ostream& out, Add const& self );
std::ostream& operator <<( std::ostream& out, TToE const& self );
std::ostream& operator <<( std::ostream& out, Num const& self );
std::ostream& operator <<( std::ostream& out, Paren const& self );
std::ostream& operator <<( std::ostream& out, Mul const& self );
std::ostream& operator <<( std::ostream& out, FToT const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
};

class Node2 {
public:
  E content;
  explicit Node2( E const& content_ );
};

class Node3 {
public:
  T content;
  explicit Node3( T const& content_ );
};

class Node4 {
public:
  explicit Node4();
};

class Node5 {
public:
  explicit Node5();
};

class Node6 {
public:
  E content;
  explicit Node6( E const& content_ );
};

class Node7 {
public:
  F content;
  explicit Node7( F const& content_ );
};

class Node8 {
public:
  F content;
  explicit Node8( F const& content_ );
};

class Node9 {
public:
  explicit Node9();
};

class Node10 {
public:
  T content;
  explicit Node10( T const& content_ );
};

class Node11 {
public:
  Int arg1
  explicit Node11( Int const& arg1_ );
};

class Node12 {
public:
  explicit Node12();
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
  auto add();
  auto num( Int const& arg1 );
  auto lp();
  auto rp();
  auto mul();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto add_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto num_transition( std::shared_ptr< State< Stack... > > const& src, Int const& arg1 );

template< typename... Stack >
auto lp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto rp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto mul_transition( std::shared_ptr< State< Stack... > > const& src );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example2.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

