
#ifndef __EXAMPLE4_HPP__

#include <memory>
#include <string>
#include <iostream>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example4 (Start) {
//   nameString : Name -> str
//   simpleHello : Start -> "hello"
//   helloWithName : Start -> "hello" Name
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace example4 {

// AST node abstract classes

class Name {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Name() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

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

class NameString : public Name, public std::tuple< std::string > {
public:
  explicit NameString( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SimpleHello : public Start, public std::tuple<  > {
public:
  explicit SimpleHello();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class HelloWithName : public Start, public std::tuple< std::shared_ptr< Name > > {
public:
  explicit HelloWithName( std::shared_ptr< Name > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class Name::Visitor{
public:
  virtual void visitNameString( NameString& host ) = 0;
};

class Name::ConstVisitor{
public:
  virtual void visitNameString( NameString const& host ) = 0;
};

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

std::ostream& operator <<( std::ostream& out, Name const& self );
std::ostream& operator <<( std::ostream& out, Start const& self );
std::ostream& operator <<( std::ostream& out, NameString const& self );
std::ostream& operator <<( std::ostream& out, SimpleHello const& self );
std::ostream& operator <<( std::ostream& out, HelloWithName const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class S1 {
public:
  std::shared_ptr< Start > content;
  explicit S1( std::shared_ptr< Start > const& content_ );
};

class S2 {
public:
  explicit S2();
};

class S3 {
public:
  std::string content;
  explicit S3( std::string const& content_ );
};

class S4 {
public:
  explicit S4();
};

class S5 {
public:
  std::shared_ptr< Name > content;
  explicit S5( std::shared_ptr< Name > const& content_ );
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
  auto hello();
  auto str( std::string const& value );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto hello_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto str_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& value );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< S2 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example4.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

