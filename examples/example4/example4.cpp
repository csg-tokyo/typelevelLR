
#include "example4.hpp"

namespace example4 {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Start::~Start() noexcept {}
Name::~Name() noexcept {}
Str::~Str() noexcept {}


SimpleHello::SimpleHello(  )
  :std::tuple<  >(  ) {}

void SimpleHello::accept( Visitor& visitor ) {
  visitor.visitSimpleHello( *this );
}
void SimpleHello::accept( ConstVisitor& visitor ) const {
  visitor.visitSimpleHello( *this );
}


HelloWithName::HelloWithName( Name const& arg1 )
  :std::tuple< Name >( arg1 ) {}

void HelloWithName::accept( Visitor& visitor ) {
  visitor.visitHelloWithName( *this );
}
void HelloWithName::accept( ConstVisitor& visitor ) const {
  visitor.visitHelloWithName( *this );
}


NameString::NameString( Str const& arg1 )
  :std::tuple< Str >( arg1 ) {}

void NameString::accept( Visitor& visitor ) {
  visitor.visitNameString( *this );
}
void NameString::accept( ConstVisitor& visitor ) const {
  visitor.visitNameString( *this );
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

std::ostream& operator <<( std::ostream &out, Str const& self ) {
  class Visitor : public Str::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
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

std::ostream& operator <<( std::ostream& out, NameString const& self ) {
  out << "NameString("<<  std::get< 0 >( self ) << ")";
  return out;
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( Start const& content_ ) :content( content_ ) {}

Node3::Node3( Name const& content_ ) :content( content_ ) {}

Node4::Node4() {}

Node5::Node5( str const& content_ ) :content( content_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

