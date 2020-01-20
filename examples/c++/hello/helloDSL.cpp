
#include "helloDSL.hpp"

namespace helloDSL {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Start::~Start() noexcept {}


SimpleHello::SimpleHello(  )
  :std::tuple<  >(  ) {}

void SimpleHello::accept( Visitor& visitor ) {
  visitor.visitSimpleHello( *this );
}
void SimpleHello::accept( ConstVisitor& visitor ) const {
  visitor.visitSimpleHello( *this );
}


HelloWithName::HelloWithName( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void HelloWithName::accept( Visitor& visitor ) {
  visitor.visitHelloWithName( *this );
}
void HelloWithName::accept( ConstVisitor& visitor ) const {
  visitor.visitHelloWithName( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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


std::ostream& operator <<( std::ostream& out, SimpleHello const& self ) {
  out << "SimpleHello(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, HelloWithName const& self ) {
  out << "HelloWithName("<<  std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( std::shared_ptr< Start > const& arg1_)
    :arg1( arg1_ ) {}

Node3::Node3( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node4::Node4() {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

