#include "helloDSL.hpp"

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template< typename Head, typename... Tail >
State< Head, Tail... >::State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ )
  :head( head_ ), tail( tail_ ) {}

template< typename Head, typename... Tail >
std::shared_ptr< State< Head, Tail... > > State< Head, Tail... >::make( Head const& head, std::shared_ptr< State< Tail... > > const& tail ) {
  std::shared_ptr< State< Head, Tail... > > result( new State< Head, Tail... >( head, tail ) );
  result->this_ = result;
  return result;
}


template< typename Head, typename... Tail >
auto State< Head, Tail... >::end() {
  return end_transition( this_.lock() );
}

template< typename Head, typename... Tail >
auto State< Head, Tail... >::hello() {
  return hello_transition( this_.lock() );
}

template< typename Head, typename... Tail >
auto State< Head, Tail... >::name( std::string const& arg1 ) {
  return name_transition( this_.lock(), arg1 );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transitions

template< typename... Tail >
auto end_transition( std::shared_ptr< State< Node2, Tail... > > const& src ) {
  return src->head.arg1;
}

template< typename... Tail >
auto hello_transition( std::shared_ptr< State< Node1, Tail... > > const& src ) {
  return State< Node4, Node1, Tail... >::make( Node4(  ), src );
}

template< typename... Tail >
auto end_transition( std::shared_ptr< State< Node3, Node4, Node1, Tail... > > const& src ) {
  std::string const& x1 = src->head.arg1;
  std::shared_ptr< Start > const& content = std::shared_ptr< Start >( new HelloWithName( x1 ) );
  std::shared_ptr< State< Node1, Tail... > > const& tail = src->tail->tail;
  return end_transition( State< Node2, Node1, Tail... >::make( Node2( content ), tail ) );
}

template< typename... Tail >
auto name_transition( std::shared_ptr< State< Node4, Tail... > > const& src, std::string const& arg1 ) {
  return State< Node3, Node4, Tail... >::make( Node3( arg1 ), src );
}

template< typename... Tail >
auto end_transition( std::shared_ptr< State< Node4, Node1, Tail... > > const& src ) {
  std::shared_ptr< Start > const& content = std::shared_ptr< Start >( new SimpleHello(  ) );
  std::shared_ptr< State< Node1, Tail... > > const& tail = src->tail;
  return end_transition( State< Node2, Node1, Tail... >::make( Node2( content ), tail ) );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
