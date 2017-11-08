
#include "example2.hpp"

namespace example2 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

E::~E() noexcept {}
F::~F() noexcept {}
T::~T() noexcept {}


Add::Add( std::shared_ptr< E > const& arg1, std::shared_ptr< T > const& arg2 )
  :std::tuple< std::shared_ptr< E >, std::shared_ptr< T > >( arg1, arg2 ) {}

void Add::accept( Visitor& visitor ) {
  visitor.visitAdd( *this );
}
void Add::accept( ConstVisitor& visitor ) const {
  visitor.visitAdd( *this );
}


TToE::TToE( std::shared_ptr< T > const& arg1 )
  :std::tuple< std::shared_ptr< T > >( arg1 ) {}

void TToE::accept( Visitor& visitor ) {
  visitor.visitTToE( *this );
}
void TToE::accept( ConstVisitor& visitor ) const {
  visitor.visitTToE( *this );
}


Num::Num( int const& arg1 )
  :std::tuple< int >( arg1 ) {}

void Num::accept( Visitor& visitor ) {
  visitor.visitNum( *this );
}
void Num::accept( ConstVisitor& visitor ) const {
  visitor.visitNum( *this );
}


Paren::Paren( std::shared_ptr< E > const& arg1 )
  :std::tuple< std::shared_ptr< E > >( arg1 ) {}

void Paren::accept( Visitor& visitor ) {
  visitor.visitParen( *this );
}
void Paren::accept( ConstVisitor& visitor ) const {
  visitor.visitParen( *this );
}


Mul::Mul( std::shared_ptr< T > const& arg1, std::shared_ptr< F > const& arg2 )
  :std::tuple< std::shared_ptr< T >, std::shared_ptr< F > >( arg1, arg2 ) {}

void Mul::accept( Visitor& visitor ) {
  visitor.visitMul( *this );
}
void Mul::accept( ConstVisitor& visitor ) const {
  visitor.visitMul( *this );
}


FToT::FToT( std::shared_ptr< F > const& arg1 )
  :std::tuple< std::shared_ptr< F > >( arg1 ) {}

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
  out << "Add("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TToE const& self ) {
  out << "TToE("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Num const& self ) {
  out << "Num("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Paren const& self ) {
  out << "Paren("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Mul const& self ) {
  out << "Mul("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FToT const& self ) {
  out << "FToT("<< *std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

S1::S1( std::shared_ptr< E > const& content_ ) :content( content_ ) {}

S2::S2() {}

S3::S3( std::shared_ptr< T > const& content_ ) :content( content_ ) {}

S4::S4() {}

S5::S5() {}

S6::S6( std::shared_ptr< E > const& content_ ) :content( content_ ) {}

S7::S7( std::shared_ptr< T > const& content_ ) :content( content_ ) {}

S8::S8( int const& content_ ) :content( content_ ) {}

S9::S9() {}

S10::S10() {}

S11::S11( std::shared_ptr< F > const& content_ ) :content( content_ ) {}

S12::S12( std::shared_ptr< F > const& content_ ) :content( content_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< S2 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< S2 >::make( S2(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

