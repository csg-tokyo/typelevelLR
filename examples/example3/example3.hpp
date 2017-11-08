
#ifndef __EXAMPLE3_HPP__

#include <memory>
#include <string>
#include <iostream>

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax example3 (S) {
//   accept : Mod0 -> eps
//   rule00 : Mod0 -> "O" Mod0
//   rule01 : Mod0 -> "I" Mod1
//   rule10 : Mod1 -> "O" Mod2
//   rule11 : Mod1 -> "I" Mod3
//   rule20 : Mod2 -> "O" Mod4
//   rule21 : Mod2 -> "I" Mod5
//   rule30 : Mod3 -> "O" Mod6
//   rule31 : Mod3 -> "I" Mod0
//   rule40 : Mod4 -> "O" Mod1
//   rule41 : Mod4 -> "I" Mod2
//   rule50 : Mod5 -> "O" Mod3
//   rule51 : Mod5 -> "I" Mod4
//   rule60 : Mod6 -> "O" Mod5
//   rule61 : Mod6 -> "I" Mod6
//   ruleS0 : S -> "O" Mod0
//   ruleS1 : S -> "I" Mod1
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace example3 {

// AST node abstract classes

class Mod0 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod0() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod1 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod1() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod2 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod2() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod3 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod3() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod4 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod4() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod5 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod5() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Mod6 {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Mod6() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class S {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~S() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class Accept : public Mod0, public std::tuple<  > {
public:
  explicit Accept();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule00 : public Mod0, public std::tuple< std::shared_ptr< Mod0 > > {
public:
  explicit Rule00( std::shared_ptr< Mod0 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule01 : public Mod0, public std::tuple< std::shared_ptr< Mod1 > > {
public:
  explicit Rule01( std::shared_ptr< Mod1 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule10 : public Mod1, public std::tuple< std::shared_ptr< Mod2 > > {
public:
  explicit Rule10( std::shared_ptr< Mod2 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule11 : public Mod1, public std::tuple< std::shared_ptr< Mod3 > > {
public:
  explicit Rule11( std::shared_ptr< Mod3 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule20 : public Mod2, public std::tuple< std::shared_ptr< Mod4 > > {
public:
  explicit Rule20( std::shared_ptr< Mod4 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule21 : public Mod2, public std::tuple< std::shared_ptr< Mod5 > > {
public:
  explicit Rule21( std::shared_ptr< Mod5 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule30 : public Mod3, public std::tuple< std::shared_ptr< Mod6 > > {
public:
  explicit Rule30( std::shared_ptr< Mod6 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule31 : public Mod3, public std::tuple< std::shared_ptr< Mod0 > > {
public:
  explicit Rule31( std::shared_ptr< Mod0 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule40 : public Mod4, public std::tuple< std::shared_ptr< Mod1 > > {
public:
  explicit Rule40( std::shared_ptr< Mod1 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule41 : public Mod4, public std::tuple< std::shared_ptr< Mod2 > > {
public:
  explicit Rule41( std::shared_ptr< Mod2 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule50 : public Mod5, public std::tuple< std::shared_ptr< Mod3 > > {
public:
  explicit Rule50( std::shared_ptr< Mod3 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule51 : public Mod5, public std::tuple< std::shared_ptr< Mod4 > > {
public:
  explicit Rule51( std::shared_ptr< Mod4 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Rule60 : public Mod6, public std::tuple< std::shared_ptr< Mod5 > > {
public:
  explicit Rule60( std::shared_ptr< Mod5 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class Rule61 : public Mod6, public std::tuple< std::shared_ptr< Mod6 > > {
public:
  explicit Rule61( std::shared_ptr< Mod6 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RuleS0 : public S, public std::tuple< std::shared_ptr< Mod0 > > {
public:
  explicit RuleS0( std::shared_ptr< Mod0 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class RuleS1 : public S, public std::tuple< std::shared_ptr< Mod1 > > {
public:
  explicit RuleS1( std::shared_ptr< Mod1 > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

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

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Mod0 const& self );
std::ostream& operator <<( std::ostream& out, Mod1 const& self );
std::ostream& operator <<( std::ostream& out, Mod2 const& self );
std::ostream& operator <<( std::ostream& out, Mod3 const& self );
std::ostream& operator <<( std::ostream& out, Mod4 const& self );
std::ostream& operator <<( std::ostream& out, Mod5 const& self );
std::ostream& operator <<( std::ostream& out, Mod6 const& self );
std::ostream& operator <<( std::ostream& out, S const& self );
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
std::ostream& operator <<( std::ostream& out, RuleS0 const& self );
std::ostream& operator <<( std::ostream& out, RuleS1 const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class S1 {
public:
  std::shared_ptr< S > content;
  explicit S1( std::shared_ptr< S > const& content_ );
};

class S2 {
public:
  explicit S2();
};

class S3 {
public:
  explicit S3();
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
  std::shared_ptr< Mod0 > content;
  explicit S6( std::shared_ptr< Mod0 > const& content_ );
};

class S7 {
public:
  std::shared_ptr< Mod1 > content;
  explicit S7( std::shared_ptr< Mod1 > const& content_ );
};

class S8 {
public:
  explicit S8();
};

class S9 {
public:
  std::shared_ptr< Mod2 > content;
  explicit S9( std::shared_ptr< Mod2 > const& content_ );
};

class S10 {
public:
  explicit S10();
};

class S11 {
public:
  explicit S11();
};

class S12 {
public:
  explicit S12();
};

class S13 {
public:
  std::shared_ptr< Mod3 > content;
  explicit S13( std::shared_ptr< Mod3 > const& content_ );
};

class S14 {
public:
  explicit S14();
};

class S15 {
public:
  std::shared_ptr< Mod4 > content;
  explicit S15( std::shared_ptr< Mod4 > const& content_ );
};

class S16 {
public:
  explicit S16();
};

class S17 {
public:
  explicit S17();
};

class S18 {
public:
  std::shared_ptr< Mod5 > content;
  explicit S18( std::shared_ptr< Mod5 > const& content_ );
};

class S19 {
public:
  explicit S19();
};

class S20 {
public:
  std::shared_ptr< Mod6 > content;
  explicit S20( std::shared_ptr< Mod6 > const& content_ );
};

class S21 {
public:
  explicit S21();
};

class S22 {
public:
  explicit S22();
};

class S23 {
public:
  std::shared_ptr< Mod0 > content;
  explicit S23( std::shared_ptr< Mod0 > const& content_ );
};

class S24 {
public:
  std::shared_ptr< Mod1 > content;
  explicit S24( std::shared_ptr< Mod1 > const& content_ );
};

class S25 {
public:
  explicit S25();
};

class S26 {
public:
  std::shared_ptr< Mod2 > content;
  explicit S26( std::shared_ptr< Mod2 > const& content_ );
};

class S27 {
public:
  std::shared_ptr< Mod3 > content;
  explicit S27( std::shared_ptr< Mod3 > const& content_ );
};

class S28 {
public:
  explicit S28();
};

class S29 {
public:
  std::shared_ptr< Mod4 > content;
  explicit S29( std::shared_ptr< Mod4 > const& content_ );
};

class S30 {
public:
  std::shared_ptr< Mod5 > content;
  explicit S30( std::shared_ptr< Mod5 > const& content_ );
};

class S31 {
public:
  explicit S31();
};

class S32 {
public:
  std::shared_ptr< Mod6 > content;
  explicit S32( std::shared_ptr< Mod6 > const& content_ );
};

class S33 {
public:
  std::shared_ptr< Mod0 > content;
  explicit S33( std::shared_ptr< Mod0 > const& content_ );
};

class S34 {
public:
  std::shared_ptr< Mod1 > content;
  explicit S34( std::shared_ptr< Mod1 > const& content_ );
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
  auto I();
  auto O();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto I_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto O_transition( std::shared_ptr< State< Stack... > > const& src );


template< typename... Stack >
auto reduce( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< S2 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "example3.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

