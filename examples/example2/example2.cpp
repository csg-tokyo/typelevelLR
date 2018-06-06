
#include "example2.hpp"

namespace example2 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

E::~E() noexcept {}
F::~F() noexcept {}
T::~T() noexcept {}


Add::Add( E const& arg1, T const& arg2 )
  :std::tuple< E, T >( arg1, arg2 ) {}

void Add::accept( Visitor& visitor ) {
  visitor.visitAdd( *this );
}
void Add::accept( ConstVisitor& visitor ) const {
  visitor.visitAdd( *this );
}


TToE::TToE( T const& arg1 )
  :std::tuple< T >( arg1 ) {}

void TToE::accept( Visitor& visitor ) {
  visitor.visitTToE( *this );
}
void TToE::accept( ConstVisitor& visitor ) const {
  visitor.visitTToE( *this );
}


Num::Num( Int const& arg1 )
  :std::tuple< Int >( arg1 ) {}

void Num::accept( Visitor& visitor ) {
  visitor.visitNum( *this );
}
void Num::accept( ConstVisitor& visitor ) const {
  visitor.visitNum( *this );
}


Paren::Paren( E const& arg1 )
  :std::tuple< E >( arg1 ) {}

void Paren::accept( Visitor& visitor ) {
  visitor.visitParen( *this );
}
void Paren::accept( ConstVisitor& visitor ) const {
  visitor.visitParen( *this );
}


Mul::Mul( T const& arg1, F const& arg2 )
  :std::tuple< T, F >( arg1, arg2 ) {}

void Mul::accept( Visitor& visitor ) {
  visitor.visitMul( *this );
}
void Mul::accept( ConstVisitor& visitor ) const {
  visitor.visitMul( *this );
}


FToT::FToT( F const& arg1 )
  :std::tuple< F >( arg1 ) {}

void FToT::accept( Visitor& visitor ) {
  visitor.visitFToT( *this );
}
void FToT::accept( ConstVisitor& visitor ) const {
  visitor.visitFToT( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, E const& self ) {
  class Visitor : public E::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAdd( Add const& host ) {
      *out_ << host;
    }
    void visitTToE( TToE const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, F const& self ) {
  class Visitor : public F::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNum( Num const& host ) {
      *out_ << host;
    }
    void visitParen( Paren const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, T const& self ) {
  class Visitor : public T::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitMul( Mul const& host ) {
      *out_ << host;
    }
    void visitFToT( FToT const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, Add const& self ) {
  out << "Add("<<  std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TToE const& self ) {
  out << "TToE("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Num const& self ) {
  out << "Num("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Paren const& self ) {
  out << "Paren("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Mul const& self ) {
  out << "Mul("<<  std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FToT const& self ) {
  out << "FToT("<<  std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( E const& content_ ) :content( content_ ) {}

Node3::Node3( T const& content_ ) :content( content_ ) {}

Node4::Node4() {}

Node5::Node5() {}

Node6::Node6( E const& content_ ) :content( content_ ) {}

Node7::Node7( F const& content_ ) :content( content_ ) {}

Node8::Node8( F const& content_ ) :content( content_ ) {}

Node9::Node9() {}

Node10::Node10( T const& content_ ) :content( content_ ) {}

Node11::Node11( Int const& arg1_ ) :arg1( arg1_ ){}

Node12::Node12() {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

