
#ifndef __EXAMPLE3_HPP__
#define __EXAMPLE3_HPP__

#include <memory>
#include <string>
#include <iostream>

namespace example3 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example3 (S) {
//   ruleS0 : S -> "O()" Mod0
//   ruleS1 : S -> "I()" Mod1
//   accept : Mod0 -> eps
//   rule00 : Mod0 -> "O()" Mod0
//   rule01 : Mod0 -> "I()" Mod1
//   rule10 : Mod1 -> "O()" Mod2
//   rule11 : Mod1 -> "I()" Mod3
//   rule20 : Mod2 -> "O()" Mod4
//   rule21 : Mod2 -> "I()" Mod5
//   rule30 : Mod3 -> "O()" Mod6
//   rule31 : Mod3 -> "I()" Mod0
//   rule40 : Mod4 -> "O()" Mod1
//   rule41 : Mod4 -> "I()" Mod2
//   rule50 : Mod5 -> "O()" Mod3
//   rule51 : Mod5 -> "I()" Mod4
//   rule60 : Mod6 -> "O()" Mod5
//   rule61 : Mod6 -> "I()" Mod6
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class S {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~S() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod0 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod0() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod1 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod1() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod2 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod2() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod3 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod3() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod4 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod4() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod5 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod5() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod6 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod6() noexcept {}

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class RuleS0 : public S, public std::tuple< Mod0 > {
public:
  explicit RuleS0( Mod0 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class RuleS1 : public S, public std::tuple< Mod1 > {
public:
  explicit RuleS1( Mod1 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Accept : public Mod0, public std::tuple<  > {
public:
  explicit Accept();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule00 : public Mod0, public std::tuple< Mod0 > {
public:
  explicit Rule00( Mod0 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule01 : public Mod0, public std::tuple< Mod1 > {
public:
  explicit Rule01( Mod1 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule10 : public Mod1, public std::tuple< Mod2 > {
public:
  explicit Rule10( Mod2 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule11 : public Mod1, public std::tuple< Mod3 > {
public:
  explicit Rule11( Mod3 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule20 : public Mod2, public std::tuple< Mod4 > {
public:
  explicit Rule20( Mod4 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule21 : public Mod2, public std::tuple< Mod5 > {
public:
  explicit Rule21( Mod5 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule30 : public Mod3, public std::tuple< Mod6 > {
public:
  explicit Rule30( Mod6 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule31 : public Mod3, public std::tuple< Mod0 > {
public:
  explicit Rule31( Mod0 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule40 : public Mod4, public std::tuple< Mod1 > {
public:
  explicit Rule40( Mod1 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule41 : public Mod4, public std::tuple< Mod2 > {
public:
  explicit Rule41( Mod2 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule50 : public Mod5, public std::tuple< Mod3 > {
public:
  explicit Rule50( Mod3 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule51 : public Mod5, public std::tuple< Mod4 > {
public:
  explicit Rule51( Mod4 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule60 : public Mod6, public std::tuple< Mod5 > {
public:
  explicit Rule60( Mod5 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule61 : public Mod6, public std::tuple< Mod6 > {
public:
  explicit Rule61( Mod6 const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class S::Visitor{
public:
  virtual void visitRuleS0( RuleS0& host ) = 0;
  virtual void visitRuleS1( RuleS1& host ) = 0;
};

class S::ConstVisitor{
public:
  virtual void visitRuleS0( RuleS0 const& host ) = 0;
  virtual void visitRuleS1( RuleS1 const& host ) = 0;
};

class Mod0::Visitor{
public:
  virtual void visitAccept( Accept& host ) = 0;
  virtual void visitRule00( Rule00& host ) = 0;
  virtual void visitRule01( Rule01& host ) = 0;
};

class Mod0::ConstVisitor{
public:
  virtual void visitAccept( Accept const& host ) = 0;
  virtual void visitRule00( Rule00 const& host ) = 0;
  virtual void visitRule01( Rule01 const& host ) = 0;
};

class Mod1::Visitor{
public:
  virtual void visitRule10( Rule10& host ) = 0;
  virtual void visitRule11( Rule11& host ) = 0;
};

class Mod1::ConstVisitor{
public:
  virtual void visitRule10( Rule10 const& host ) = 0;
  virtual void visitRule11( Rule11 const& host ) = 0;
};

class Mod2::Visitor{
public:
  virtual void visitRule20( Rule20& host ) = 0;
  virtual void visitRule21( Rule21& host ) = 0;
};

class Mod2::ConstVisitor{
public:
  virtual void visitRule20( Rule20 const& host ) = 0;
  virtual void visitRule21( Rule21 const& host ) = 0;
};

class Mod3::Visitor{
public:
  virtual void visitRule30( Rule30& host ) = 0;
  virtual void visitRule31( Rule31& host ) = 0;
};

class Mod3::ConstVisitor{
public:
  virtual void visitRule30( Rule30 const& host ) = 0;
  virtual void visitRule31( Rule31 const& host ) = 0;
};

class Mod4::Visitor{
public:
  virtual void visitRule40( Rule40& host ) = 0;
  virtual void visitRule41( Rule41& host ) = 0;
};

class Mod4::ConstVisitor{
public:
  virtual void visitRule40( Rule40 const& host ) = 0;
  virtual void visitRule41( Rule41 const& host ) = 0;
};

class Mod5::Visitor{
public:
  virtual void visitRule50( Rule50& host ) = 0;
  virtual void visitRule51( Rule51& host ) = 0;
};

class Mod5::ConstVisitor{
public:
  virtual void visitRule50( Rule50 const& host ) = 0;
  virtual void visitRule51( Rule51 const& host ) = 0;
};

class Mod6::Visitor{
public:
  virtual void visitRule60( Rule60& host ) = 0;
  virtual void visitRule61( Rule61& host ) = 0;
};

class Mod6::ConstVisitor{
public:
  virtual void visitRule60( Rule60 const& host ) = 0;
  virtual void visitRule61( Rule61 const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, S const& self );
std::ostream& operator <<( std::ostream& out, Mod0 const& self );
std::ostream& operator <<( std::ostream& out, Mod1 const& self );
std::ostream& operator <<( std::ostream& out, Mod2 const& self );
std::ostream& operator <<( std::ostream& out, Mod3 const& self );
std::ostream& operator <<( std::ostream& out, Mod4 const& self );
std::ostream& operator <<( std::ostream& out, Mod5 const& self );
std::ostream& operator <<( std::ostream& out, Mod6 const& self );
std::ostream& operator <<( std::ostream& out, RuleS0 const& self );
std::ostream& operator <<( std::ostream& out, RuleS1 const& self );
std::ostream& operator <<( std::ostream& out, Accept const& self );
std::ostream& operator <<( std::ostream& out, Rule00 const& self );
std::ostream& operator <<( std::ostream& out, Rule01 const& self );
std::ostream& operator <<( std::ostream& out, Rule10 const& self );
std::ostream& operator <<( std::ostream& out, Rule11 const& self );
std::ostream& operator <<( std::ostream& out, Rule20 const& self );
std::ostream& operator <<( std::ostream& out, Rule21 const& self );
std::ostream& operator <<( std::ostream& out, Rule30 const& self );
std::ostream& operator <<( std::ostream& out, Rule31 const& self );
std::ostream& operator <<( std::ostream& out, Rule40 const& self );
std::ostream& operator <<( std::ostream& out, Rule41 const& self );
std::ostream& operator <<( std::ostream& out, Rule50 const& self );
std::ostream& operator <<( std::ostream& out, Rule51 const& self );
std::ostream& operator <<( std::ostream& out, Rule60 const& self );
std::ostream& operator <<( std::ostream& out, Rule61 const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
};

class Node2 {
public:
  S content;
  explicit Node2( S const& content_ );
};

class Node3 {
public:
  explicit Node3();
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
  Mod0 content;
  explicit Node6( Mod0 const& content_ );
};

class Node7 {
public:
  Mod1 content;
  explicit Node7( Mod1 const& content_ );
};

class Node8 {
public:
  explicit Node8();
};

class Node9 {
public:
  Mod2 content;
  explicit Node9( Mod2 const& content_ );
};

class Node10 {
public:
  explicit Node10();
};

class Node11 {
public:
  explicit Node11();
};

class Node12 {
public:
  explicit Node12();
};

class Node13 {
public:
  Mod3 content;
  explicit Node13( Mod3 const& content_ );
};

class Node14 {
public:
  explicit Node14();
};

class Node15 {
public:
  Mod4 content;
  explicit Node15( Mod4 const& content_ );
};

class Node16 {
public:
  explicit Node16();
};

class Node17 {
public:
  explicit Node17();
};

class Node18 {
public:
  Mod5 content;
  explicit Node18( Mod5 const& content_ );
};

class Node19 {
public:
  explicit Node19();
};

class Node20 {
public:
  Mod6 content;
  explicit Node20( Mod6 const& content_ );
};

class Node21 {
public:
  explicit Node21();
};

class Node22 {
public:
  explicit Node22();
};

class Node23 {
public:
  Mod0 content;
  explicit Node23( Mod0 const& content_ );
};

class Node24 {
public:
  Mod1 content;
  explicit Node24( Mod1 const& content_ );
};

class Node25 {
public:
  explicit Node25();
};

class Node26 {
public:
  Mod2 content;
  explicit Node26( Mod2 const& content_ );
};

class Node27 {
public:
  Mod3 content;
  explicit Node27( Mod3 const& content_ );
};

class Node28 {
public:
  explicit Node28();
};

class Node29 {
public:
  Mod4 content;
  explicit Node29( Mod4 const& content_ );
};

class Node30 {
public:
  Mod5 content;
  explicit Node30( Mod5 const& content_ );
};

class Node31 {
public:
  explicit Node31();
};

class Node32 {
public:
  Mod6 content;
  explicit Node32( Mod6 const& content_ );
};

class Node33 {
public:
  Mod0 content;
  explicit Node33( Mod0 const& content_ );
};

class Node34 {
public:
  Mod1 content;
  explicit Node34( Mod1 const& content_ );
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
  auto O();
  auto I();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto O_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto I_transition( std::shared_ptr< State< Stack... > > const& src );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example3.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

