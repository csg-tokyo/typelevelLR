
#include "example1.hpp"

namespace example1 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

A::~A() noexcept {}
B::~B() noexcept {}


GenA::GenA( A const& arg1 )
  :std::tuple< A >( arg1 ) {}

void GenA::accept( Visitor& visitor ) {
  visitor.visitGenA( *this );
}
void GenA::accept( ConstVisitor& visitor ) const {
  visitor.visitGenA( *this );
}


BToA::BToA( B const& arg1 )
  :std::tuple< B >( arg1 ) {}

void BToA::accept( Visitor& visitor ) {
  visitor.visitBToA( *this );
}
void BToA::accept( ConstVisitor& visitor ) const {
  visitor.visitBToA( *this );
}


GenAB::GenAB( B const& arg1 )
  :std::tuple< B >( arg1 ) {}

void GenAB::accept( Visitor& visitor ) {
  visitor.visitGenAB( *this );
}
void GenAB::accept( ConstVisitor& visitor ) const {
  visitor.visitGenAB( *this );
}


Fin::Fin(  )
  :std::tuple<  >(  ) {}

void Fin::accept( Visitor& visitor ) {
  visitor.visitFin( *this );
}
void Fin::accept( ConstVisitor& visitor ) const {
  visitor.visitFin( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, A const& self ) {
  class Visitor : public A::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitGenA( GenA const& host ) {
      *out_ << host;
    }
    void visitBToA( BToA const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, B const& self ) {
  class Visitor : public B::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitGenAB( GenAB const& host ) {
      *out_ << host;
    }
    void visitFin( Fin const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, GenA const& self ) {
  out << "GenA("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BToA const& self ) {
  out << "BToA("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, GenAB const& self ) {
  out << "GenAB("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Fin const& self ) {
  out << "Fin(" << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( A const& content_ ) :content( content_ ) {}

Node3::Node3( B const& content_ ) :content( content_ ) {}

Node4::Node4( B const& content_ ) :content( content_ ) {}

Node5::Node5() {}

Node6::Node6( A const& content_ ) :content( content_ ) {}

Node7::Node7() {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

