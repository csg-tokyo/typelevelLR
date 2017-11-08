
#ifndef __EXAMPLE2_HPP__

#include <memory>
#include <string>
#include <iostream>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example2 (E) {
//   Add : E -> E "add" T
//   TToE : E -> T
//   Num : F -> int
//   Paren : F -> "lp" E "rp"
//   Mul : T -> T "mul" F
//   FToT : T -> F
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace example2 {

// AST node abstract classes

class E {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~E() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class F {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~F() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class T {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~T() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class Add : public E, public std::tuple< std::shared_ptr< E >, std::shared_ptr< T > > {
public:
  explicit Add( std::shared_ptr< E > const& arg1, std::shared_ptr< T > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TToE : public E, public std::tuple< std::shared_ptr< T > > {
public:
  explicit TToE( std::shared_ptr< T > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Num : public F, public std::tuple< int > {
public:
  explicit Num( int const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Paren : public F, public std::tuple< std::shared_ptr< E > > {
public:
  explicit Paren( std::shared_ptr< E > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Mul : public T, public std::tuple< std::shared_ptr< T >, std::shared_ptr< F > > {
public:
  explicit Mul( std::shared_ptr< T > const& arg1, std::shared_ptr< F > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FToT : public T, public std::tuple< std::shared_ptr< F > > {
public:
  explicit FToT( std::shared_ptr< F > const& arg1 );

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

class S1 {
public:
  std::shared_ptr< E > content;
  explicit S1( std::shared_ptr< E > const& content_ );
};

class S2 {
public:
  explicit S2();
};

class S3 {
public:
  std::shared_ptr< T > content;
  explicit S3( std::shared_ptr< T > const& content_ );
};

class S4 {
public:
  explicit S4();
};

class S5 {
public:
  explicit S5();
};

class S6 {
public:
  std::shared_ptr< E > content;
  explicit S6( std::shared_ptr< E > const& content_ );
};

class S7 {
public:
  std::shared_ptr< T > content;
  explicit S7( std::shared_ptr< T > const& content_ );
};

class S8 {
public:
  int content;
  explicit S8( int const& content_ );
};

class S9 {
public:
  explicit S9();
};

class S10 {
public:
  explicit S10();
};

class S11 {
public:
  std::shared_ptr< F > content;
  explicit S11( std::shared_ptr< F > const& content_ );
};

class S12 {
public:
  std::shared_ptr< F > content;
  explicit S12( std::shared_ptr< F > const& content_ );
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
  std::shared_ptr< State< Head, Tail... > > this_;
  Head head;
  std::shared_ptr< State< Tail... > > tail;

private:
  State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ );

public:
static std::shared_ptr< State< Head, Tail... > > make( Head const& head, std::shared_ptr< State< Tail... > > const& tail );

public:
  auto end();
  auto add();
  auto lp();
  auto mul();
  auto rp();
  auto num( int const& value );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto add_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto lp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto mul_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto rp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto num_transition( std::shared_ptr< State< Stack... > > const& src, int const& value );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< S2 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example2.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

