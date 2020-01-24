
#ifndef ___SYNTAX__HPP__
#define ___SYNTAX__HPP__

#include <memory>
#include <string>
#include <iostream>

namespace syntax {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax syntax (Syntax) {
//   DefineSyntax : Syntax -> "syntax(std::string)" "startsWith(std::string)" Rules
//   RuleDerive : Rule -> "rule(std::string)" "derive(std::string)" RuleBody
//   RuleBodyTo : RuleBody -> "to(std::string)" RuleTail
//   RuleBodyToEpsilon : RuleBody -> "toEpsilon()"
//   RuleTailTo : RuleTail -> "andThen(std::string)" RuleTail
//   RuleTailEpsilon : RuleTail -> eps
//   RulesCons : Rules -> Rule Rules
//   RulesNull : Rules -> eps
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class Syntax {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Syntax() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Rule {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Rule() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class RuleBody {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~RuleBody() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class RuleTail {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~RuleTail() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Rules {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Rules() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class DefineSyntax : public Syntax, public std::tuple< std::string, std::string, std::shared_ptr< Rules > > {
public:
  explicit DefineSyntax( std::string const& arg1, std::string const& arg2, std::shared_ptr< Rules > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RuleDerive : public Rule, public std::tuple< std::string, std::string, std::shared_ptr< RuleBody > > {
public:
  explicit RuleDerive( std::string const& arg1, std::string const& arg2, std::shared_ptr< RuleBody > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RuleBodyTo : public RuleBody, public std::tuple< std::string, std::shared_ptr< RuleTail > > {
public:
  explicit RuleBodyTo( std::string const& arg1, std::shared_ptr< RuleTail > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class RuleBodyToEpsilon : public RuleBody, public std::tuple<  > {
public:
  explicit RuleBodyToEpsilon();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RuleTailTo : public RuleTail, public std::tuple< std::string, std::shared_ptr< RuleTail > > {
public:
  explicit RuleTailTo( std::string const& arg1, std::shared_ptr< RuleTail > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class RuleTailEpsilon : public RuleTail, public std::tuple<  > {
public:
  explicit RuleTailEpsilon();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RulesCons : public Rules, public std::tuple< std::shared_ptr< Rule >, std::shared_ptr< Rules > > {
public:
  explicit RulesCons( std::shared_ptr< Rule > const& arg1, std::shared_ptr< Rules > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class RulesNull : public Rules, public std::tuple<  > {
public:
  explicit RulesNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class Syntax::Visitor{
public:
  virtual void visitDefineSyntax( DefineSyntax& host ) = 0;
};

class Syntax::ConstVisitor{
public:
  virtual void visitDefineSyntax( DefineSyntax const& host ) = 0;
};

class Rule::Visitor{
public:
  virtual void visitRuleDerive( RuleDerive& host ) = 0;
};

class Rule::ConstVisitor{
public:
  virtual void visitRuleDerive( RuleDerive const& host ) = 0;
};

class RuleBody::Visitor{
public:
  virtual void visitRuleBodyTo( RuleBodyTo& host ) = 0;
  virtual void visitRuleBodyToEpsilon( RuleBodyToEpsilon& host ) = 0;
};

class RuleBody::ConstVisitor{
public:
  virtual void visitRuleBodyTo( RuleBodyTo const& host ) = 0;
  virtual void visitRuleBodyToEpsilon( RuleBodyToEpsilon const& host ) = 0;
};

class RuleTail::Visitor{
public:
  virtual void visitRuleTailTo( RuleTailTo& host ) = 0;
  virtual void visitRuleTailEpsilon( RuleTailEpsilon& host ) = 0;
};

class RuleTail::ConstVisitor{
public:
  virtual void visitRuleTailTo( RuleTailTo const& host ) = 0;
  virtual void visitRuleTailEpsilon( RuleTailEpsilon const& host ) = 0;
};

class Rules::Visitor{
public:
  virtual void visitRulesCons( RulesCons& host ) = 0;
  virtual void visitRulesNull( RulesNull& host ) = 0;
};

class Rules::ConstVisitor{
public:
  virtual void visitRulesCons( RulesCons const& host ) = 0;
  virtual void visitRulesNull( RulesNull const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Syntax const& self );
std::ostream& operator <<( std::ostream& out, Rule const& self );
std::ostream& operator <<( std::ostream& out, RuleBody const& self );
std::ostream& operator <<( std::ostream& out, RuleTail const& self );
std::ostream& operator <<( std::ostream& out, Rules const& self );
std::ostream& operator <<( std::ostream& out, DefineSyntax const& self );
std::ostream& operator <<( std::ostream& out, RuleDerive const& self );
std::ostream& operator <<( std::ostream& out, RuleBodyTo const& self );
std::ostream& operator <<( std::ostream& out, RuleBodyToEpsilon const& self );
std::ostream& operator <<( std::ostream& out, RuleTailTo const& self );
std::ostream& operator <<( std::ostream& out, RuleTailEpsilon const& self );
std::ostream& operator <<( std::ostream& out, RulesCons const& self );
std::ostream& operator <<( std::ostream& out, RulesNull const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
  explicit Node1();
};

class Node2 {
public:
  std::shared_ptr< Syntax > arg1;
  explicit Node2( std::shared_ptr< Syntax > const& arg1_ );
};

class Node3 {
public:
  std::shared_ptr< Rules > arg1;
  explicit Node3( std::shared_ptr< Rules > const& arg1_ );
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
  std::shared_ptr< RuleTail > arg1;
  explicit Node6( std::shared_ptr< RuleTail > const& arg1_ );
};

class Node7 {
public:
  std::string arg1;
  explicit Node7( std::string const& arg1_ );
};

class Node8 {
public:
  std::string arg1;
  explicit Node8( std::string const& arg1_ );
};

class Node9 {
public:
  explicit Node9();
};

class Node10 {
public:
  std::shared_ptr< RuleBody > arg1;
  explicit Node10( std::shared_ptr< RuleBody > const& arg1_ );
};

class Node11 {
public:
  std::string arg1;
  explicit Node11( std::string const& arg1_ );
};

class Node12 {
public:
  std::shared_ptr< Rule > arg1;
  explicit Node12( std::shared_ptr< Rule > const& arg1_ );
};

class Node13 {
public:
  std::string arg1;
  explicit Node13( std::string const& arg1_ );
};

class Node14 {
public:
  std::shared_ptr< RuleTail > arg1;
  explicit Node14( std::shared_ptr< RuleTail > const& arg1_ );
};

class Node15 {
public:
  std::shared_ptr< Rules > arg1;
  explicit Node15( std::shared_ptr< Rules > const& arg1_ );
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
  auto rule( std::string const& arg1 );
  auto derive( std::string const& arg1 );
  auto to( std::string const& arg1 );
  auto toEpsilon();
  auto andThen( std::string const& arg1 );
  auto syntax( std::string const& arg1 );
  auto startsWith( std::string const& arg1 );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto rule_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto derive_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto to_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto toEpsilon_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto andThen_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto syntax_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto startsWith_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "syntax.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

