
#include "syntax.hpp"

namespace syntax {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Syntax::~Syntax() noexcept {}
Rule::~Rule() noexcept {}
RuleBody::~RuleBody() noexcept {}
RuleTail::~RuleTail() noexcept {}
Rules::~Rules() noexcept {}


DefineSyntax::DefineSyntax( std::string const& arg1, std::string const& arg2, std::shared_ptr< Rules > const& arg3 )
  :std::tuple< std::string, std::string, std::shared_ptr< Rules > >( arg1, arg2, arg3 ) {}

void DefineSyntax::accept( Visitor& visitor ) {
  visitor.visitDefineSyntax( *this );
}
void DefineSyntax::accept( ConstVisitor& visitor ) const {
  visitor.visitDefineSyntax( *this );
}


RuleDerive::RuleDerive( std::string const& arg1, std::string const& arg2, std::shared_ptr< RuleBody > const& arg3 )
  :std::tuple< std::string, std::string, std::shared_ptr< RuleBody > >( arg1, arg2, arg3 ) {}

void RuleDerive::accept( Visitor& visitor ) {
  visitor.visitRuleDerive( *this );
}
void RuleDerive::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleDerive( *this );
}


RuleBodyTo::RuleBodyTo( std::string const& arg1, std::shared_ptr< RuleTail > const& arg2 )
  :std::tuple< std::string, std::shared_ptr< RuleTail > >( arg1, arg2 ) {}

void RuleBodyTo::accept( Visitor& visitor ) {
  visitor.visitRuleBodyTo( *this );
}
void RuleBodyTo::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleBodyTo( *this );
}


RuleBodyToEpsilon::RuleBodyToEpsilon(  )
  :std::tuple<  >(  ) {}

void RuleBodyToEpsilon::accept( Visitor& visitor ) {
  visitor.visitRuleBodyToEpsilon( *this );
}
void RuleBodyToEpsilon::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleBodyToEpsilon( *this );
}


RuleTailTo::RuleTailTo( std::string const& arg1, std::shared_ptr< RuleTail > const& arg2 )
  :std::tuple< std::string, std::shared_ptr< RuleTail > >( arg1, arg2 ) {}

void RuleTailTo::accept( Visitor& visitor ) {
  visitor.visitRuleTailTo( *this );
}
void RuleTailTo::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleTailTo( *this );
}


RuleTailEpsilon::RuleTailEpsilon(  )
  :std::tuple<  >(  ) {}

void RuleTailEpsilon::accept( Visitor& visitor ) {
  visitor.visitRuleTailEpsilon( *this );
}
void RuleTailEpsilon::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleTailEpsilon( *this );
}


RulesCons::RulesCons( std::shared_ptr< Rule > const& arg1, std::shared_ptr< Rules > const& arg2 )
  :std::tuple< std::shared_ptr< Rule >, std::shared_ptr< Rules > >( arg1, arg2 ) {}

void RulesCons::accept( Visitor& visitor ) {
  visitor.visitRulesCons( *this );
}
void RulesCons::accept( ConstVisitor& visitor ) const {
  visitor.visitRulesCons( *this );
}


RulesNull::RulesNull(  )
  :std::tuple<  >(  ) {}

void RulesNull::accept( Visitor& visitor ) {
  visitor.visitRulesNull( *this );
}
void RulesNull::accept( ConstVisitor& visitor ) const {
  visitor.visitRulesNull( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, Syntax const& self ) {
  class Visitor : public Syntax::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitDefineSyntax( DefineSyntax const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Rule const& self ) {
  class Visitor : public Rule::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRuleDerive( RuleDerive const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, RuleBody const& self ) {
  class Visitor : public RuleBody::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRuleBodyTo( RuleBodyTo const& host ) {
      *out_ << host;
    }
    void visitRuleBodyToEpsilon( RuleBodyToEpsilon const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, RuleTail const& self ) {
  class Visitor : public RuleTail::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRuleTailTo( RuleTailTo const& host ) {
      *out_ << host;
    }
    void visitRuleTailEpsilon( RuleTailEpsilon const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Rules const& self ) {
  class Visitor : public Rules::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRulesCons( RulesCons const& host ) {
      *out_ << host;
    }
    void visitRulesNull( RulesNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, DefineSyntax const& self ) {
  out << "DefineSyntax("<<  std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleDerive const& self ) {
  out << "RuleDerive("<<  std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleBodyTo const& self ) {
  out << "RuleBodyTo("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleBodyToEpsilon const& self ) {
  out << "RuleBodyToEpsilon(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleTailTo const& self ) {
  out << "RuleTailTo("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleTailEpsilon const& self ) {
  out << "RuleTailEpsilon(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RulesCons const& self ) {
  out << "RulesCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RulesNull const& self ) {
  out << "RulesNull(" << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( std::shared_ptr< Syntax > const& arg1_)
    :arg1( arg1_ ) {}

Node3::Node3( std::shared_ptr< Rules > const& arg1_)
    :arg1( arg1_ ) {}

Node4::Node4( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node5::Node5( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node6::Node6( std::shared_ptr< RuleTail > const& arg1_)
    :arg1( arg1_ ) {}

Node7::Node7( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node8::Node8( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node9::Node9() {}

Node10::Node10( std::shared_ptr< RuleBody > const& arg1_)
    :arg1( arg1_ ) {}

Node11::Node11( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node12::Node12( std::shared_ptr< Rule > const& arg1_)
    :arg1( arg1_ ) {}

Node13::Node13( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node14::Node14( std::shared_ptr< RuleTail > const& arg1_)
    :arg1( arg1_ ) {}

Node15::Node15( std::shared_ptr< Rules > const& arg1_)
    :arg1( arg1_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

