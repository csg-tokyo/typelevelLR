
#include "example3.hpp"

namespace example3 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

S::~S() noexcept {}
Mod0::~Mod0() noexcept {}
Mod1::~Mod1() noexcept {}
Mod2::~Mod2() noexcept {}
Mod3::~Mod3() noexcept {}
Mod4::~Mod4() noexcept {}
Mod5::~Mod5() noexcept {}
Mod6::~Mod6() noexcept {}


RuleS0::RuleS0( Mod0 const& arg1 )
  :std::tuple< Mod0 >( arg1 ) {}

void RuleS0::accept( Visitor& visitor ) {
  visitor.visitRuleS0( *this );
}
void RuleS0::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleS0( *this );
}


RuleS1::RuleS1( Mod1 const& arg1 )
  :std::tuple< Mod1 >( arg1 ) {}

void RuleS1::accept( Visitor& visitor ) {
  visitor.visitRuleS1( *this );
}
void RuleS1::accept( ConstVisitor& visitor ) const {
  visitor.visitRuleS1( *this );
}


Accept::Accept(  )
  :std::tuple<  >(  ) {}

void Accept::accept( Visitor& visitor ) {
  visitor.visitAccept( *this );
}
void Accept::accept( ConstVisitor& visitor ) const {
  visitor.visitAccept( *this );
}


Rule00::Rule00( Mod0 const& arg1 )
  :std::tuple< Mod0 >( arg1 ) {}

void Rule00::accept( Visitor& visitor ) {
  visitor.visitRule00( *this );
}
void Rule00::accept( ConstVisitor& visitor ) const {
  visitor.visitRule00( *this );
}


Rule01::Rule01( Mod1 const& arg1 )
  :std::tuple< Mod1 >( arg1 ) {}

void Rule01::accept( Visitor& visitor ) {
  visitor.visitRule01( *this );
}
void Rule01::accept( ConstVisitor& visitor ) const {
  visitor.visitRule01( *this );
}


Rule10::Rule10( Mod2 const& arg1 )
  :std::tuple< Mod2 >( arg1 ) {}

void Rule10::accept( Visitor& visitor ) {
  visitor.visitRule10( *this );
}
void Rule10::accept( ConstVisitor& visitor ) const {
  visitor.visitRule10( *this );
}


Rule11::Rule11( Mod3 const& arg1 )
  :std::tuple< Mod3 >( arg1 ) {}

void Rule11::accept( Visitor& visitor ) {
  visitor.visitRule11( *this );
}
void Rule11::accept( ConstVisitor& visitor ) const {
  visitor.visitRule11( *this );
}


Rule20::Rule20( Mod4 const& arg1 )
  :std::tuple< Mod4 >( arg1 ) {}

void Rule20::accept( Visitor& visitor ) {
  visitor.visitRule20( *this );
}
void Rule20::accept( ConstVisitor& visitor ) const {
  visitor.visitRule20( *this );
}


Rule21::Rule21( Mod5 const& arg1 )
  :std::tuple< Mod5 >( arg1 ) {}

void Rule21::accept( Visitor& visitor ) {
  visitor.visitRule21( *this );
}
void Rule21::accept( ConstVisitor& visitor ) const {
  visitor.visitRule21( *this );
}


Rule30::Rule30( Mod6 const& arg1 )
  :std::tuple< Mod6 >( arg1 ) {}

void Rule30::accept( Visitor& visitor ) {
  visitor.visitRule30( *this );
}
void Rule30::accept( ConstVisitor& visitor ) const {
  visitor.visitRule30( *this );
}


Rule31::Rule31( Mod0 const& arg1 )
  :std::tuple< Mod0 >( arg1 ) {}

void Rule31::accept( Visitor& visitor ) {
  visitor.visitRule31( *this );
}
void Rule31::accept( ConstVisitor& visitor ) const {
  visitor.visitRule31( *this );
}


Rule40::Rule40( Mod1 const& arg1 )
  :std::tuple< Mod1 >( arg1 ) {}

void Rule40::accept( Visitor& visitor ) {
  visitor.visitRule40( *this );
}
void Rule40::accept( ConstVisitor& visitor ) const {
  visitor.visitRule40( *this );
}


Rule41::Rule41( Mod2 const& arg1 )
  :std::tuple< Mod2 >( arg1 ) {}

void Rule41::accept( Visitor& visitor ) {
  visitor.visitRule41( *this );
}
void Rule41::accept( ConstVisitor& visitor ) const {
  visitor.visitRule41( *this );
}


Rule50::Rule50( Mod3 const& arg1 )
  :std::tuple< Mod3 >( arg1 ) {}

void Rule50::accept( Visitor& visitor ) {
  visitor.visitRule50( *this );
}
void Rule50::accept( ConstVisitor& visitor ) const {
  visitor.visitRule50( *this );
}


Rule51::Rule51( Mod4 const& arg1 )
  :std::tuple< Mod4 >( arg1 ) {}

void Rule51::accept( Visitor& visitor ) {
  visitor.visitRule51( *this );
}
void Rule51::accept( ConstVisitor& visitor ) const {
  visitor.visitRule51( *this );
}


Rule60::Rule60( Mod5 const& arg1 )
  :std::tuple< Mod5 >( arg1 ) {}

void Rule60::accept( Visitor& visitor ) {
  visitor.visitRule60( *this );
}
void Rule60::accept( ConstVisitor& visitor ) const {
  visitor.visitRule60( *this );
}


Rule61::Rule61( Mod6 const& arg1 )
  :std::tuple< Mod6 >( arg1 ) {}

void Rule61::accept( Visitor& visitor ) {
  visitor.visitRule61( *this );
}
void Rule61::accept( ConstVisitor& visitor ) const {
  visitor.visitRule61( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, S const& self ) {
  class Visitor : public S::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRuleS0( RuleS0 const& host ) {
      *out_ << host;
    }
    void visitRuleS1( RuleS1 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod0 const& self ) {
  class Visitor : public Mod0::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAccept( Accept const& host ) {
      *out_ << host;
    }
    void visitRule00( Rule00 const& host ) {
      *out_ << host;
    }
    void visitRule01( Rule01 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod1 const& self ) {
  class Visitor : public Mod1::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule10( Rule10 const& host ) {
      *out_ << host;
    }
    void visitRule11( Rule11 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod2 const& self ) {
  class Visitor : public Mod2::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule20( Rule20 const& host ) {
      *out_ << host;
    }
    void visitRule21( Rule21 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod3 const& self ) {
  class Visitor : public Mod3::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule30( Rule30 const& host ) {
      *out_ << host;
    }
    void visitRule31( Rule31 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod4 const& self ) {
  class Visitor : public Mod4::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule40( Rule40 const& host ) {
      *out_ << host;
    }
    void visitRule41( Rule41 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod5 const& self ) {
  class Visitor : public Mod5::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule50( Rule50 const& host ) {
      *out_ << host;
    }
    void visitRule51( Rule51 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Mod6 const& self ) {
  class Visitor : public Mod6::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRule60( Rule60 const& host ) {
      *out_ << host;
    }
    void visitRule61( Rule61 const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, RuleS0 const& self ) {
  out << "RuleS0("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RuleS1 const& self ) {
  out << "RuleS1("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Accept const& self ) {
  out << "Accept(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule00 const& self ) {
  out << "Rule00("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule01 const& self ) {
  out << "Rule01("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule10 const& self ) {
  out << "Rule10("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule11 const& self ) {
  out << "Rule11("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule20 const& self ) {
  out << "Rule20("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule21 const& self ) {
  out << "Rule21("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule30 const& self ) {
  out << "Rule30("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule31 const& self ) {
  out << "Rule31("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule40 const& self ) {
  out << "Rule40("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule41 const& self ) {
  out << "Rule41("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule50 const& self ) {
  out << "Rule50("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule51 const& self ) {
  out << "Rule51("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule60 const& self ) {
  out << "Rule60("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Rule61 const& self ) {
  out << "Rule61("<<  std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( S const& content_ ) :content( content_ ) {}

Node3::Node3() {}

Node4::Node4() {}

Node5::Node5() {}

Node6::Node6( Mod0 const& content_ ) :content( content_ ) {}

Node7::Node7( Mod1 const& content_ ) :content( content_ ) {}

Node8::Node8() {}

Node9::Node9( Mod2 const& content_ ) :content( content_ ) {}

Node10::Node10() {}

Node11::Node11() {}

Node12::Node12() {}

Node13::Node13( Mod3 const& content_ ) :content( content_ ) {}

Node14::Node14() {}

Node15::Node15( Mod4 const& content_ ) :content( content_ ) {}

Node16::Node16() {}

Node17::Node17() {}

Node18::Node18( Mod5 const& content_ ) :content( content_ ) {}

Node19::Node19() {}

Node20::Node20( Mod6 const& content_ ) :content( content_ ) {}

Node21::Node21() {}

Node22::Node22() {}

Node23::Node23( Mod0 const& content_ ) :content( content_ ) {}

Node24::Node24( Mod1 const& content_ ) :content( content_ ) {}

Node25::Node25() {}

Node26::Node26( Mod2 const& content_ ) :content( content_ ) {}

Node27::Node27( Mod3 const& content_ ) :content( content_ ) {}

Node28::Node28() {}

Node29::Node29( Mod4 const& content_ ) :content( content_ ) {}

Node30::Node30( Mod5 const& content_ ) :content( content_ ) {}

Node31::Node31() {}

Node32::Node32( Mod6 const& content_ ) :content( content_ ) {}

Node33::Node33( Mod0 const& content_ ) :content( content_ ) {}

Node34::Node34( Mod1 const& content_ ) :content( content_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

