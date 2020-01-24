
#include "DotLanguage.hpp"

namespace DotLanguage {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Graph::~Graph() noexcept {}
Ands::~Ands() noexcept {}
EdgeAttr::~EdgeAttr() noexcept {}
EdgeAttrs::~EdgeAttrs() noexcept {}
NodeAttr::~NodeAttr() noexcept {}
NodeAttrs::~NodeAttrs() noexcept {}
Stmt::~Stmt() noexcept {}
Stmts::~Stmts() noexcept {}


Directed::Directed( std::string const& arg1, std::shared_ptr< Stmts > const& arg2 )
  :std::tuple< std::string, std::shared_ptr< Stmts > >( arg1, arg2 ) {}

void Directed::accept( Visitor& visitor ) {
  visitor.visitDirected( *this );
}
void Directed::accept( ConstVisitor& visitor ) const {
  visitor.visitDirected( *this );
}


Undirected::Undirected( std::string const& arg1, std::shared_ptr< Stmts > const& arg2 )
  :std::tuple< std::string, std::shared_ptr< Stmts > >( arg1, arg2 ) {}

void Undirected::accept( Visitor& visitor ) {
  visitor.visitUndirected( *this );
}
void Undirected::accept( ConstVisitor& visitor ) const {
  visitor.visitUndirected( *this );
}


AndsCons::AndsCons( std::shared_ptr< Ands > const& arg1, std::string const& arg2 )
  :std::tuple< std::shared_ptr< Ands >, std::string >( arg1, arg2 ) {}

void AndsCons::accept( Visitor& visitor ) {
  visitor.visitAndsCons( *this );
}
void AndsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitAndsCons( *this );
}


AndsNull::AndsNull(  )
  :std::tuple<  >(  ) {}

void AndsNull::accept( Visitor& visitor ) {
  visitor.visitAndsNull( *this );
}
void AndsNull::accept( ConstVisitor& visitor ) const {
  visitor.visitAndsNull( *this );
}


EdgeAttrColor::EdgeAttrColor( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void EdgeAttrColor::accept( Visitor& visitor ) {
  visitor.visitEdgeAttrColor( *this );
}
void EdgeAttrColor::accept( ConstVisitor& visitor ) const {
  visitor.visitEdgeAttrColor( *this );
}


EdgeAttrStyle::EdgeAttrStyle( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void EdgeAttrStyle::accept( Visitor& visitor ) {
  visitor.visitEdgeAttrStyle( *this );
}
void EdgeAttrStyle::accept( ConstVisitor& visitor ) const {
  visitor.visitEdgeAttrStyle( *this );
}


EdgeAttrsCons::EdgeAttrsCons( std::shared_ptr< EdgeAttrs > const& arg1, std::shared_ptr< EdgeAttr > const& arg2 )
  :std::tuple< std::shared_ptr< EdgeAttrs >, std::shared_ptr< EdgeAttr > >( arg1, arg2 ) {}

void EdgeAttrsCons::accept( Visitor& visitor ) {
  visitor.visitEdgeAttrsCons( *this );
}
void EdgeAttrsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitEdgeAttrsCons( *this );
}


EdgeAttrsNull::EdgeAttrsNull(  )
  :std::tuple<  >(  ) {}

void EdgeAttrsNull::accept( Visitor& visitor ) {
  visitor.visitEdgeAttrsNull( *this );
}
void EdgeAttrsNull::accept( ConstVisitor& visitor ) const {
  visitor.visitEdgeAttrsNull( *this );
}


NodeAttrColor::NodeAttrColor( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void NodeAttrColor::accept( Visitor& visitor ) {
  visitor.visitNodeAttrColor( *this );
}
void NodeAttrColor::accept( ConstVisitor& visitor ) const {
  visitor.visitNodeAttrColor( *this );
}


NodeAttrShape::NodeAttrShape( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void NodeAttrShape::accept( Visitor& visitor ) {
  visitor.visitNodeAttrShape( *this );
}
void NodeAttrShape::accept( ConstVisitor& visitor ) const {
  visitor.visitNodeAttrShape( *this );
}


NodeAttrsCons::NodeAttrsCons( std::shared_ptr< NodeAttrs > const& arg1, std::shared_ptr< NodeAttr > const& arg2 )
  :std::tuple< std::shared_ptr< NodeAttrs >, std::shared_ptr< NodeAttr > >( arg1, arg2 ) {}

void NodeAttrsCons::accept( Visitor& visitor ) {
  visitor.visitNodeAttrsCons( *this );
}
void NodeAttrsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitNodeAttrsCons( *this );
}


NodeAttrsNull::NodeAttrsNull(  )
  :std::tuple<  >(  ) {}

void NodeAttrsNull::accept( Visitor& visitor ) {
  visitor.visitNodeAttrsNull( *this );
}
void NodeAttrsNull::accept( ConstVisitor& visitor ) const {
  visitor.visitNodeAttrsNull( *this );
}


NodeStmt::NodeStmt( std::string const& arg1, std::shared_ptr< Ands > const& arg2, std::shared_ptr< NodeAttrs > const& arg3 )
  :std::tuple< std::string, std::shared_ptr< Ands >, std::shared_ptr< NodeAttrs > >( arg1, arg2, arg3 ) {}

void NodeStmt::accept( Visitor& visitor ) {
  visitor.visitNodeStmt( *this );
}
void NodeStmt::accept( ConstVisitor& visitor ) const {
  visitor.visitNodeStmt( *this );
}


EdgeStmt::EdgeStmt( std::string const& arg1, std::shared_ptr< Ands > const& arg2, std::string const& arg3, std::shared_ptr< Ands > const& arg4, std::shared_ptr< EdgeAttrs > const& arg5 )
  :std::tuple< std::string, std::shared_ptr< Ands >, std::string, std::shared_ptr< Ands >, std::shared_ptr< EdgeAttrs > >( arg1, arg2, arg3, arg4, arg5 ) {}

void EdgeStmt::accept( Visitor& visitor ) {
  visitor.visitEdgeStmt( *this );
}
void EdgeStmt::accept( ConstVisitor& visitor ) const {
  visitor.visitEdgeStmt( *this );
}


StmtsCons::StmtsCons( std::shared_ptr< Stmts > const& arg1, std::shared_ptr< Stmt > const& arg2 )
  :std::tuple< std::shared_ptr< Stmts >, std::shared_ptr< Stmt > >( arg1, arg2 ) {}

void StmtsCons::accept( Visitor& visitor ) {
  visitor.visitStmtsCons( *this );
}
void StmtsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitStmtsCons( *this );
}


StmtsNull::StmtsNull(  )
  :std::tuple<  >(  ) {}

void StmtsNull::accept( Visitor& visitor ) {
  visitor.visitStmtsNull( *this );
}
void StmtsNull::accept( ConstVisitor& visitor ) const {
  visitor.visitStmtsNull( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, Graph const& self ) {
  class Visitor : public Graph::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitDirected( Directed const& host ) {
      *out_ << host;
    }
    void visitUndirected( Undirected const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Ands const& self ) {
  class Visitor : public Ands::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAndsCons( AndsCons const& host ) {
      *out_ << host;
    }
    void visitAndsNull( AndsNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, EdgeAttr const& self ) {
  class Visitor : public EdgeAttr::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitEdgeAttrColor( EdgeAttrColor const& host ) {
      *out_ << host;
    }
    void visitEdgeAttrStyle( EdgeAttrStyle const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, EdgeAttrs const& self ) {
  class Visitor : public EdgeAttrs::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitEdgeAttrsCons( EdgeAttrsCons const& host ) {
      *out_ << host;
    }
    void visitEdgeAttrsNull( EdgeAttrsNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, NodeAttr const& self ) {
  class Visitor : public NodeAttr::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNodeAttrColor( NodeAttrColor const& host ) {
      *out_ << host;
    }
    void visitNodeAttrShape( NodeAttrShape const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, NodeAttrs const& self ) {
  class Visitor : public NodeAttrs::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNodeAttrsCons( NodeAttrsCons const& host ) {
      *out_ << host;
    }
    void visitNodeAttrsNull( NodeAttrsNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Stmt const& self ) {
  class Visitor : public Stmt::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNodeStmt( NodeStmt const& host ) {
      *out_ << host;
    }
    void visitEdgeStmt( EdgeStmt const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Stmts const& self ) {
  class Visitor : public Stmts::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitStmtsCons( StmtsCons const& host ) {
      *out_ << host;
    }
    void visitStmtsNull( StmtsNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, Directed const& self ) {
  out << "Directed("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Undirected const& self ) {
  out << "Undirected("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AndsCons const& self ) {
  out << "AndsCons("<< *std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AndsNull const& self ) {
  out << "AndsNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, EdgeAttrColor const& self ) {
  out << "EdgeAttrColor("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, EdgeAttrStyle const& self ) {
  out << "EdgeAttrStyle("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, EdgeAttrsCons const& self ) {
  out << "EdgeAttrsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, EdgeAttrsNull const& self ) {
  out << "EdgeAttrsNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NodeAttrColor const& self ) {
  out << "NodeAttrColor("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NodeAttrShape const& self ) {
  out << "NodeAttrShape("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NodeAttrsCons const& self ) {
  out << "NodeAttrsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NodeAttrsNull const& self ) {
  out << "NodeAttrsNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NodeStmt const& self ) {
  out << "NodeStmt("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, EdgeStmt const& self ) {
  out << "EdgeStmt("<<  std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<<  std::get< 2 >( self ) << ", "<< *std::get< 3 >( self ) << ", "<< *std::get< 4 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, StmtsCons const& self ) {
  out << "StmtsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, StmtsNull const& self ) {
  out << "StmtsNull(" << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( std::shared_ptr< Graph > const& arg1_)
    :arg1( arg1_ ) {}

Node3::Node3( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node4::Node4( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node5::Node5( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node6::Node6( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node7::Node7( std::shared_ptr< Ands > const& arg1_)
    :arg1( arg1_ ) {}

Node8::Node8( std::shared_ptr< Ands > const& arg1_)
    :arg1( arg1_ ) {}

Node9::Node9( std::shared_ptr< Ands > const& arg1_)
    :arg1( arg1_ ) {}

Node10::Node10( std::shared_ptr< Stmts > const& arg1_)
    :arg1( arg1_ ) {}

Node11::Node11( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node12::Node12( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node13::Node13( std::shared_ptr< EdgeAttrs > const& arg1_)
    :arg1( arg1_ ) {}

Node14::Node14( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node15::Node15( std::shared_ptr< EdgeAttr > const& arg1_)
    :arg1( arg1_ ) {}

Node16::Node16( std::shared_ptr< Stmts > const& arg1_)
    :arg1( arg1_ ) {}

Node17::Node17( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node18::Node18( std::shared_ptr< NodeAttrs > const& arg1_)
    :arg1( arg1_ ) {}

Node19::Node19( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node20::Node20( std::shared_ptr< NodeAttr > const& arg1_)
    :arg1( arg1_ ) {}

Node21::Node21( std::shared_ptr< Stmt > const& arg1_)
    :arg1( arg1_ ) {}

Node22::Node22( std::string const& arg1_)
    :arg1( arg1_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

