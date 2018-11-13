
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar

// Directed : Graph -> "digraph(String)" Stmts
// Undirected : Graph -> "graph(String)" Stmts
// AndsCons : Ands -> Ands "and(String)"
// AndsNull : Ands -> eps
// EdgeAttrColor : EdgeAttr -> "color(String)"
// EdgeAttrStyle : EdgeAttr -> "style(String)"
// EdgeAttrsCons : EdgeAttrs -> EdgeAttrs EdgeAttr
// EdgeAttrsNull : EdgeAttrs -> eps
// NodeAttrColor : NodeAttr -> "color(String)"
// NodeAttrShape : NodeAttr -> "shape(String)"
// NodeAttrsCons : NodeAttrs -> NodeAttrs NodeAttr
// NodeAttrsNull : NodeAttrs -> eps
// NodeStmt : Stmt -> "node(String)" Ands NodeAttrs
// EdgeStmt : Stmt -> "edge(String)" Ands "to(String)" Ands EdgeAttrs
// StmtsCons : Stmts -> Stmts Stmt
// StmtsNull : Stmts -> eps

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object exam2 {

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // AST

  trait Graph
  case class Directed ( arg1 : String, arg2 : Stmts ) extends Graph
  case class Undirected ( arg1 : String, arg2 : Stmts ) extends Graph

  trait Ands
  case class AndsCons ( arg1 : Ands, arg2 : String ) extends Ands
  case class AndsNull (  ) extends Ands

  trait EdgeAttr
  case class EdgeAttrColor ( arg1 : String ) extends EdgeAttr
  case class EdgeAttrStyle ( arg1 : String ) extends EdgeAttr

  trait EdgeAttrs
  case class EdgeAttrsCons ( arg1 : EdgeAttrs, arg2 : EdgeAttr ) extends EdgeAttrs
  case class EdgeAttrsNull (  ) extends EdgeAttrs

  trait NodeAttr
  case class NodeAttrColor ( arg1 : String ) extends NodeAttr
  case class NodeAttrShape ( arg1 : String ) extends NodeAttr

  trait NodeAttrs
  case class NodeAttrsCons ( arg1 : NodeAttrs, arg2 : NodeAttr ) extends NodeAttrs
  case class NodeAttrsNull (  ) extends NodeAttrs

  trait Stmt
  case class NodeStmt ( arg1 : String, arg2 : Ands, arg3 : NodeAttrs ) extends Stmt
  case class EdgeStmt ( arg1 : String, arg2 : Ands, arg3 : String, arg4 : Ands, arg5 : EdgeAttrs ) extends Stmt

  trait Stmts
  case class StmtsCons ( arg1 : Stmts, arg2 : Stmt ) extends Stmts
  case class StmtsNull (  ) extends Stmts

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition type classes

  trait AndTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait ColorTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait StyleTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait DigraphTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait GraphTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait ShapeTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait NodeTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait EdgeTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait ToTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait EndTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }


  // implicit classes for transition methods

  implicit class AndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : AndTransition[ Src, Dst ] ) {
    def and( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class ColorTransitable [ Src, Dst ] ( src : Src ) ( implicit t : ColorTransition[ Src, Dst ] ) {
    def color( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class StyleTransitable [ Src, Dst ] ( src : Src ) ( implicit t : StyleTransition[ Src, Dst ] ) {
    def style( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class DigraphTransitable [ Src, Dst ] ( src : Src ) ( implicit t : DigraphTransition[ Src, Dst ] ) {
    def digraph( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class GraphTransitable [ Src, Dst ] ( src : Src ) ( implicit t : GraphTransition[ Src, Dst ] ) {
    def graph( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class ShapeTransitable [ Src, Dst ] ( src : Src ) ( implicit t : ShapeTransition[ Src, Dst ] ) {
    def shape( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class NodeTransitable [ Src, Dst ] ( src : Src ) ( implicit t : NodeTransition[ Src, Dst ] ) {
    def node( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class EdgeTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EdgeTransition[ Src, Dst ] ) {
    def edge( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class ToTransitable [ Src, Dst ] ( src : Src ) ( implicit t : ToTransition[ Src, Dst ] ) {
    def to( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class EndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EndTransition[ Src, Dst ] ) {
    def end() : Dst = t.transit( src )
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // stack elements

  case class Node1 [ Prev ] ( prev : Prev )

  case class Node2 [ Prev ] ( prev : Prev, arg1 : Graph )

  case class Node3 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node4 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node5 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node6 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node7 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node8 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node9 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node10 [ Prev ] ( prev : Prev, arg1 : Stmts )

  case class Node11 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node12 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node13 [ Prev ] ( prev : Prev, arg1 : EdgeAttrs )

  case class Node14 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node15 [ Prev ] ( prev : Prev, arg1 : EdgeAttr )

  case class Node16 [ Prev ] ( prev : Prev, arg1 : Stmts )

  case class Node17 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node18 [ Prev ] ( prev : Prev, arg1 : NodeAttrs )

  case class Node19 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node20 [ Prev ] ( prev : Prev, arg1 : NodeAttr )

  case class Node21 [ Prev ] ( prev : Prev, arg1 : Stmt )

  case class Node22 [ Prev ] ( prev : Prev, arg1 : String )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition implementations

  implicit def acceptNode2[ Prev ] : EndTransition[ Node2[ Prev ], Graph ] = {
    new EndTransition[ Node2[ Prev ], Graph ] {
      def transit( src : Node2[ Prev ] ) : Graph = {
        src.arg1
      }
    }
  }

  implicit def shiftNode1Digraph[ Prev ] : DigraphTransition[ Node1[ Prev ], Node11[ Node1[ Prev ] ] ] = {
    new DigraphTransition[ Node1[ Prev ], Node11[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node11[ Node1[ Prev ] ] = {
        Node11[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode1Graph[ Prev ] : GraphTransition[ Node1[ Prev ], Node22[ Node1[ Prev ] ] ] = {
    new GraphTransition[ Node1[ Prev ], Node22[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node22[ Node1[ Prev ] ] = {
        Node22[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode3And1[ Prev, Dst ] ( implicit t : AndTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : AndTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new AndTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3And2[ Prev, Dst ] ( implicit t : AndTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : AndTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new AndTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3And3[ Prev, Dst ] ( implicit t : AndTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : AndTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new AndTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Color3[ Prev, Dst ] ( implicit t : ColorTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Edge3[ Prev, Dst ] ( implicit t : EdgeTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Node3[ Prev, Dst ] ( implicit t : NodeTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Shape2[ Prev, Dst ] ( implicit t : ShapeTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Shape3[ Prev, Dst ] ( implicit t : ShapeTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Style2[ Prev, Dst ] ( implicit t : StyleTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Style3[ Prev, Dst ] ( implicit t : StyleTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3To1[ Prev, Dst ] ( implicit t : ToTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3To2[ Prev, Dst ] ( implicit t : ToTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3To3[ Prev, Dst ] ( implicit t : ToTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3End1[ Prev, Dst ] ( implicit t : EndTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node7[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node7[ Node4[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode3End2[ Prev, Dst ] ( implicit t : EndTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node8[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node8[ Node5[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode3End3[ Prev, Dst ] ( implicit t : EndTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node9[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node9[ Node6[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode4And1[ Prev, Dst ] ( implicit t : AndTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : AndTransition[ Node4[ Prev ], Dst ] = {
    new AndTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : ColorTransition[ Node4[ Prev ], Dst ] = {
    new ColorTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : EdgeTransition[ Node4[ Prev ], Dst ] = {
    new EdgeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : NodeTransition[ Node4[ Prev ], Dst ] = {
    new NodeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : ShapeTransition[ Node4[ Prev ], Dst ] = {
    new ShapeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4End1[ Prev, Dst ] ( implicit t : EndTransition[ Node7[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node4[ Prev ], Dst ] = {
    new EndTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ] ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node7[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode5And1[ Prev, Dst ] ( implicit t : AndTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : AndTransition[ Node5[ Prev ], Dst ] = {
    new AndTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : ColorTransition[ Node5[ Prev ], Dst ] = {
    new ColorTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : EdgeTransition[ Node5[ Prev ], Dst ] = {
    new EdgeTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : NodeTransition[ Node5[ Prev ], Dst ] = {
    new NodeTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : StyleTransition[ Node5[ Prev ], Dst ] = {
    new StyleTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5End1[ Prev, Dst ] ( implicit t : EndTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : EndTransition[ Node5[ Prev ], Dst ] = {
    new EndTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ] ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode6And1[ Prev, Dst ] ( implicit t : AndTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : AndTransition[ Node6[ Prev ], Dst ] = {
    new AndTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6To1[ Prev, Dst ] ( implicit t : ToTransition[ Node9[ Node6[ Prev ] ], Dst ] ) : ToTransition[ Node6[ Prev ], Dst ] = {
    new ToTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node9[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode7And[ Prev ] : AndTransition[ Node7[ Prev ], Node3[ Node7[ Prev ] ] ] = {
    new AndTransition[ Node7[ Prev ], Node3[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Node3[ Node7[ Prev ] ] = {
        Node3[ Node7[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode7Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ColorTransition[ Node7[ Prev ], Dst ] = {
    new ColorTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode7Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EdgeTransition[ Node7[ Prev ], Dst ] = {
    new EdgeTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode7Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : NodeTransition[ Node7[ Prev ], Dst ] = {
    new NodeTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode7Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ShapeTransition[ Node7[ Prev ], Dst ] = {
    new ShapeTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode7End1[ Prev, Dst ] ( implicit t : EndTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node7[ Prev ], Dst ] = {
    new EndTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ] ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode8And[ Prev ] : AndTransition[ Node8[ Prev ], Node3[ Node8[ Prev ] ] ] = {
    new AndTransition[ Node8[ Prev ], Node3[ Node8[ Prev ] ] ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Node3[ Node8[ Prev ] ] = {
        Node3[ Node8[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode8Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : ColorTransition[ Node8[ Prev ], Dst ] = {
    new ColorTransition[ Node8[ Prev ], Dst ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode8Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : EdgeTransition[ Node8[ Prev ], Dst ] = {
    new EdgeTransition[ Node8[ Prev ], Dst ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode8Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : NodeTransition[ Node8[ Prev ], Dst ] = {
    new NodeTransition[ Node8[ Prev ], Dst ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode8Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : StyleTransition[ Node8[ Prev ], Dst ] = {
    new StyleTransition[ Node8[ Prev ], Dst ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode8End1[ Prev, Dst ] ( implicit t : EndTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Prev ], Dst ] = {
    new EndTransition[ Node8[ Prev ], Dst ] {
      def transit( src : Node8[ Prev ] ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode9And[ Prev ] : AndTransition[ Node9[ Prev ], Node3[ Node9[ Prev ] ] ] = {
    new AndTransition[ Node9[ Prev ], Node3[ Node9[ Prev ] ] ] {
      def transit( src : Node9[ Prev ], arg1 : String ) : Node3[ Node9[ Prev ] ] = {
        Node3[ Node9[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode9To[ Prev ] : ToTransition[ Node9[ Prev ], Node5[ Node9[ Prev ] ] ] = {
    new ToTransition[ Node9[ Prev ], Node5[ Node9[ Prev ] ] ] {
      def transit( src : Node9[ Prev ], arg1 : String ) : Node5[ Node9[ Prev ] ] = {
        Node5[ Node9[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode10Edge[ Prev ] : EdgeTransition[ Node10[ Prev ], Node6[ Node10[ Prev ] ] ] = {
    new EdgeTransition[ Node10[ Prev ], Node6[ Node10[ Prev ] ] ] {
      def transit( src : Node10[ Prev ], arg1 : String ) : Node6[ Node10[ Prev ] ] = {
        Node6[ Node10[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode10Node[ Prev ] : NodeTransition[ Node10[ Prev ], Node4[ Node10[ Prev ] ] ] = {
    new NodeTransition[ Node10[ Prev ], Node4[ Node10[ Prev ] ] ] {
      def transit( src : Node10[ Prev ], arg1 : String ) : Node4[ Node10[ Prev ] ] = {
        Node4[ Node10[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode10End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node11[ Node1[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node10[ Node11[ Node1[ Prev ] ] ], Dst ] {
      def transit( src : Node10[ Node11[ Node1[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = Directed( src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode11Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node11[ Prev ], Dst ] = {
    new EdgeTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode11Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node11[ Prev ], Dst ] = {
    new NodeTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode11End1[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node11[ Prev ], Dst ] = {
    new EndTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ] ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode12Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : ColorTransition[ Node12[ Node13[ Prev ] ], Dst ] = {
    new ColorTransition[ Node12[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode12Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : EdgeTransition[ Node12[ Node13[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node12[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode12Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : NodeTransition[ Node12[ Node13[ Prev ] ], Dst ] = {
    new NodeTransition[ Node12[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode12Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : StyleTransition[ Node12[ Node13[ Prev ] ], Dst ] = {
    new StyleTransition[ Node12[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode12End1[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : EndTransition[ Node12[ Node13[ Prev ] ], Dst ] = {
    new EndTransition[ Node12[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node13[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode13Color[ Prev ] : ColorTransition[ Node13[ Prev ], Node12[ Node13[ Prev ] ] ] = {
    new ColorTransition[ Node13[ Prev ], Node12[ Node13[ Prev ] ] ] {
      def transit( src : Node13[ Prev ], arg1 : String ) : Node12[ Node13[ Prev ] ] = {
        Node12[ Node13[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode13Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : EdgeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] = {
    new EdgeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : EdgeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new EdgeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : NodeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] = {
    new NodeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : NodeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new NodeTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode13Style[ Prev ] : StyleTransition[ Node13[ Prev ], Node14[ Node13[ Prev ] ] ] = {
    new StyleTransition[ Node13[ Prev ], Node14[ Node13[ Prev ] ] ] {
      def transit( src : Node13[ Prev ], arg1 : String ) : Node14[ Node13[ Prev ] ] = {
        Node14[ Node13[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode13End1[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : EndTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] = {
    new EndTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node10[ Prev ] ] ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode13End2[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new EndTransition[ Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node13[ Node8[ Node5[ Node9[ Node6[ Node16[ Prev ] ] ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode14Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : ColorTransition[ Node14[ Node13[ Prev ] ], Dst ] = {
    new ColorTransition[ Node14[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node14[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : EdgeTransition[ Node14[ Node13[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node14[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node14[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : NodeTransition[ Node14[ Node13[ Prev ] ], Dst ] = {
    new NodeTransition[ Node14[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node14[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : StyleTransition[ Node14[ Node13[ Prev ] ], Dst ] = {
    new StyleTransition[ Node14[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node14[ Node13[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14End1[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node13[ Prev ] ], Dst ] ) : EndTransition[ Node14[ Node13[ Prev ] ], Dst ] = {
    new EndTransition[ Node14[ Node13[ Prev ] ], Dst ] {
      def transit( src : Node14[ Node13[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node15[ Node13[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode15Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : ColorTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node13[ Node8[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : EdgeTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node13[ Node8[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : NodeTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node13[ Node8[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : StyleTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node13[ Node8[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15End1[ Prev, Dst ] ( implicit t : EndTransition[ Node13[ Node8[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node13[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node13[ Node8[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node13[ Node8[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode16Edge[ Prev ] : EdgeTransition[ Node16[ Prev ], Node6[ Node16[ Prev ] ] ] = {
    new EdgeTransition[ Node16[ Prev ], Node6[ Node16[ Prev ] ] ] {
      def transit( src : Node16[ Prev ], arg1 : String ) : Node6[ Node16[ Prev ] ] = {
        Node6[ Node16[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode16Node[ Prev ] : NodeTransition[ Node16[ Prev ], Node4[ Node16[ Prev ] ] ] = {
    new NodeTransition[ Node16[ Prev ], Node4[ Node16[ Prev ] ] ] {
      def transit( src : Node16[ Prev ], arg1 : String ) : Node4[ Node16[ Prev ] ] = {
        Node4[ Node16[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode16End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node16[ Node22[ Node1[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node16[ Node22[ Node1[ Prev ] ] ], Dst ] {
      def transit( src : Node16[ Node22[ Node1[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = Undirected( src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode17Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : ColorTransition[ Node17[ Node18[ Prev ] ], Dst ] = {
    new ColorTransition[ Node17[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node17[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode17Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : EdgeTransition[ Node17[ Node18[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node17[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node17[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode17Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : NodeTransition[ Node17[ Node18[ Prev ] ], Dst ] = {
    new NodeTransition[ Node17[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node17[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode17Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : ShapeTransition[ Node17[ Node18[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node17[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node17[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode17End1[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : EndTransition[ Node17[ Node18[ Prev ] ], Dst ] = {
    new EndTransition[ Node17[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node17[ Node18[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode18Color[ Prev ] : ColorTransition[ Node18[ Prev ], Node17[ Node18[ Prev ] ] ] = {
    new ColorTransition[ Node18[ Prev ], Node17[ Node18[ Prev ] ] ] {
      def transit( src : Node18[ Prev ], arg1 : String ) : Node17[ Node18[ Prev ] ] = {
        Node17[ Node18[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode18Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : EdgeTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] = {
    new EdgeTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode18Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : EdgeTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] = {
    new EdgeTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode18Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : NodeTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] = {
    new NodeTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode18Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : NodeTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] = {
    new NodeTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode18Shape[ Prev ] : ShapeTransition[ Node18[ Prev ], Node19[ Node18[ Prev ] ] ] = {
    new ShapeTransition[ Node18[ Prev ], Node19[ Node18[ Prev ] ] ] {
      def transit( src : Node18[ Prev ], arg1 : String ) : Node19[ Node18[ Prev ] ] = {
        Node19[ Node18[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode18End1[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node10[ Prev ] ], Dst ] ) : EndTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node10[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node10[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode18End2[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node18[ Node7[ Node4[ Node16[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode19Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : ColorTransition[ Node19[ Node18[ Prev ] ], Dst ] = {
    new ColorTransition[ Node19[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : EdgeTransition[ Node19[ Node18[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node19[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : NodeTransition[ Node19[ Node18[ Prev ] ], Dst ] = {
    new NodeTransition[ Node19[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : ShapeTransition[ Node19[ Node18[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node19[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node18[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19End1[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node18[ Prev ] ], Dst ] ) : EndTransition[ Node19[ Node18[ Prev ] ], Dst ] = {
    new EndTransition[ Node19[ Node18[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node18[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node18[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode20Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ColorTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node20[ Node18[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode20Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EdgeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node20[ Node18[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode20Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : NodeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node20[ Node18[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode20Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ShapeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node20[ Node18[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode20End1[ Prev, Dst ] ( implicit t : EndTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node20[ Node18[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node20[ Node18[ Node7[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode21Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node10[ Node11[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode21Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : EdgeTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node16[ Node22[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode21Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node10[ Node11[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode21Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : NodeTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node16[ Node22[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode21End1[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node21[ Node10[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node10[ Node11[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode21End2[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : EndTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node21[ Node16[ Node22[ Prev ] ] ], Dst ] {
      def transit( src : Node21[ Node16[ Node22[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode22Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : EdgeTransition[ Node22[ Prev ], Dst ] = {
    new EdgeTransition[ Node22[ Prev ], Dst ] {
      def transit( src : Node22[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : NodeTransition[ Node22[ Prev ], Dst ] = {
    new NodeTransition[ Node22[ Prev ], Dst ] {
      def transit( src : Node22[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22End1[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node22[ Prev ] ], Dst ] ) : EndTransition[ Node22[ Prev ], Dst ] = {
    new EndTransition[ Node22[ Prev ], Dst ] {
      def transit( src : Node22[ Prev ] ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node16[ Node22[ Prev ] ]( prev, tree ) )
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def begin() : Node1[ Unit ] = Node1( () )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

