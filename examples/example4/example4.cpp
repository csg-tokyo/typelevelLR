
#include "example4.hpp"

namespace example4 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Name::~Name() noexcept {}
Start::~Start() noexcept {}


NameString::NameString( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void NameString::accept( Visitor& visitor ) {
  visitor.visitNameString( *this );
}
void NameString::accept( ConstVisitor& visitor ) const {
  visitor.visitNameString( *this );
}


SimpleHello::SimpleHello(  )
  :std::tuple<  >(  ) {}

void SimpleHello::accept( Visitor& visitor ) {
  visitor.visitSimpleHello( *this );
}
void SimpleHello::accept( ConstVisitor& visitor ) const {
  visitor.visitSimpleHello( *this );
}


HelloWithName::HelloWithName( std::shared_ptr< Name > const& arg1 )
  :std::tuple< std::shared_ptr< Name > >( arg1 ) {}

void HelloWithName::accept( Visitor& visitor ) {
  visitor.visitHelloWithName( *this );
}
void HelloWithName::accept( ConstVisitor& visitor ) const {
  visitor.visitHelloWithName( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, Name const& self ) {
  class Visitor : public Name::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNameString( NameString const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Start const& self ) {
  class Visitor : public Start::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSimpleHello( SimpleHello const& host ) {
      *out_ << host;
    }
    void visitHelloWithName( HelloWithName const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, NameString const& self ) {
  out << "NameString("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SimpleHello const& self ) {
  out << "SimpleHello(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, HelloWithName const& self ) {
  out << "HelloWithName("<< *std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

S1::S1( std::shared_ptr< Start > const& content_ ) :content( content_ ) {}

S2::S2() {}

S3::S3( std::string const& content_ ) :content( content_ ) {}

S4::S4() {}

S5::S5( std::shared_ptr< Name > const& content_ ) :content( content_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< S2 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< S2 >::make( S2(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

