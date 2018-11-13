
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar

// Directed : Graph -> "digraph(String)" Stmts
// Undirected : Graph -> "graph(String)" Stmts
// AndsCons : Ands -> "and(String)" Ands
// AndsNull : Ands -> eps
// EdgeAttrColor : EdgeAttr -> "color(String)"
// EdgeAttrStyle : EdgeAttr -> "style(String)"
// EdgeAttrsCons : EdgeAttrs -> EdgeAttr EdgeAttrs
// EdgeAttrsNull : EdgeAttrs -> eps
// NodeAttrColor : NodeAttr -> "color(String)"
// NodeAttrShape : NodeAttr -> "shape(String)"
// NodeAttrsCons : NodeAttrs -> NodeAttr NodeAttrs
// NodeAttrsNull : NodeAttrs -> eps
// NodeStmt : Stmt -> "node(String)" Ands NodeAttrs
// EdgeStmt : Stmt -> "edge(String)" Ands "to(String)" Ands EdgeAttrs
// StmtsCons : Stmts -> Stmt Stmts
// StmtsNull : Stmts -> eps

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object exam1 {

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // AST

  trait Graph
  case class Directed ( arg1 : String, arg2 : Stmts ) extends Graph
  case class Undirected ( arg1 : String, arg2 : Stmts ) extends Graph

  trait Ands
  case class AndsCons ( arg1 : String, arg2 : Ands ) extends Ands
  case class AndsNull (  ) extends Ands

  trait EdgeAttr
  case class EdgeAttrColor ( arg1 : String ) extends EdgeAttr
  case class EdgeAttrStyle ( arg1 : String ) extends EdgeAttr

  trait EdgeAttrs
  case class EdgeAttrsCons ( arg1 : EdgeAttr, arg2 : EdgeAttrs ) extends EdgeAttrs
  case class EdgeAttrsNull (  ) extends EdgeAttrs

  trait NodeAttr
  case class NodeAttrColor ( arg1 : String ) extends NodeAttr
  case class NodeAttrShape ( arg1 : String ) extends NodeAttr

  trait NodeAttrs
  case class NodeAttrsCons ( arg1 : NodeAttr, arg2 : NodeAttrs ) extends NodeAttrs
  case class NodeAttrsNull (  ) extends NodeAttrs

  trait Stmt
  case class NodeStmt ( arg1 : String, arg2 : Ands, arg3 : NodeAttrs ) extends Stmt
  case class EdgeStmt ( arg1 : String, arg2 : Ands, arg3 : String, arg4 : Ands, arg5 : EdgeAttrs ) extends Stmt

  trait Stmts
  case class StmtsCons ( arg1 : Stmt, arg2 : Stmts ) extends Stmts
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

  case class Node3 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node4 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node5 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node6 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node7 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node8 [ Prev ] ( prev : Prev, arg1 : Stmts )

  case class Node9 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node10 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node11 [ Prev ] ( prev : Prev, arg1 : EdgeAttr )

  case class Node12 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node13 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node14 [ Prev ] ( prev : Prev, arg1 : EdgeAttrs )

  case class Node15 [ Prev ] ( prev : Prev, arg1 : EdgeAttrs )

  case class Node16 [ Prev ] ( prev : Prev, arg1 : Stmt )

  case class Node17 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node18 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node19 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node20 [ Prev ] ( prev : Prev, arg1 : NodeAttr )

  case class Node21 [ Prev ] ( prev : Prev, arg1 : Ands )

  case class Node22 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node23 [ Prev ] ( prev : Prev, arg1 : NodeAttrs )

  case class Node24 [ Prev ] ( prev : Prev, arg1 : NodeAttrs )

  case class Node25 [ Prev ] ( prev : Prev, arg1 : Stmts )

  case class Node26 [ Prev ] ( prev : Prev, arg1 : Stmts )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition implementations

  implicit def acceptNode2[ Prev ] : EndTransition[ Node2[ Prev ], Graph ] = {
    new EndTransition[ Node2[ Prev ], Graph ] {
      def transit( src : Node2[ Prev ] ) : Graph = {
        src.arg1
      }
    }
  }

  implicit def shiftNode1Digraph[ Prev ] : DigraphTransition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] = {
    new DigraphTransition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node9[ Node1[ Prev ] ] = {
        Node9[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode1Graph[ Prev ] : GraphTransition[ Node1[ Prev ], Node17[ Node1[ Prev ] ] ] = {
    new GraphTransition[ Node1[ Prev ], Node17[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node17[ Node1[ Prev ] ] = {
        Node17[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode3Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Color3[ Prev, Dst ] ( implicit t : ColorTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Color4[ Prev, Dst ] ( implicit t : ColorTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ColorTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new ColorTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Edge3[ Prev, Dst ] ( implicit t : EdgeTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Edge4[ Prev, Dst ] ( implicit t : EdgeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EdgeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Node3[ Prev, Dst ] ( implicit t : NodeTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Node4[ Prev, Dst ] ( implicit t : NodeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : NodeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Shape2[ Prev, Dst ] ( implicit t : ShapeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Shape3[ Prev, Dst ] ( implicit t : ShapeTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Shape4[ Prev, Dst ] ( implicit t : ShapeTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ShapeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new ShapeTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Style2[ Prev, Dst ] ( implicit t : StyleTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Style3[ Prev, Dst ] ( implicit t : StyleTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3Style4[ Prev, Dst ] ( implicit t : StyleTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : StyleTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new StyleTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3To1[ Prev, Dst ] ( implicit t : ToTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3To2[ Prev, Dst ] ( implicit t : ToTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3To3[ Prev, Dst ] ( implicit t : ToTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode3To4[ Prev, Dst ] ( implicit t : ToTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ToTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new ToTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode3End1[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node4[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode3End2[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode3End3[ Prev, Dst ] ( implicit t : EndTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node6[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode3End4[ Prev, Dst ] ( implicit t : EndTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node7[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = AndsCons( src.prev.arg1, src.arg1 )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode4And[ Prev ] : AndTransition[ Node4[ Prev ], Node4[ Node4[ Prev ] ] ] = {
    new AndTransition[ Node4[ Prev ], Node4[ Node4[ Prev ] ] ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Node4[ Node4[ Prev ] ] = {
        Node4[ Node4[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode4Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ColorTransition[ Node4[ Prev ], Dst ] = {
    new ColorTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EdgeTransition[ Node4[ Prev ], Dst ] = {
    new EdgeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : NodeTransition[ Node4[ Prev ], Dst ] = {
    new NodeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ShapeTransition[ Node4[ Prev ], Dst ] = {
    new ShapeTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : StyleTransition[ Node4[ Prev ], Dst ] = {
    new StyleTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4To1[ Prev, Dst ] ( implicit t : ToTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : ToTransition[ Node4[ Prev ], Dst ] = {
    new ToTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode4End1[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node4[ Prev ], Dst ] = {
    new EndTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ] ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode5And[ Prev ] : AndTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] = {
    new AndTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Node4[ Node5[ Prev ] ] = {
        Node4[ Node5[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode5Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : ColorTransition[ Node5[ Prev ], Dst ] = {
    new ColorTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : EdgeTransition[ Node5[ Prev ], Dst ] = {
    new EdgeTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : NodeTransition[ Node5[ Prev ], Dst ] = {
    new NodeTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : ShapeTransition[ Node5[ Prev ], Dst ] = {
    new ShapeTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode5End1[ Prev, Dst ] ( implicit t : EndTransition[ Node21[ Node5[ Prev ] ], Dst ] ) : EndTransition[ Node5[ Prev ], Dst ] = {
    new EndTransition[ Node5[ Prev ], Dst ] {
      def transit( src : Node5[ Prev ] ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node21[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode6And[ Prev ] : AndTransition[ Node6[ Prev ], Node4[ Node6[ Prev ] ] ] = {
    new AndTransition[ Node6[ Prev ], Node4[ Node6[ Prev ] ] ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Node4[ Node6[ Prev ] ] = {
        Node4[ Node6[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode6Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : ColorTransition[ Node6[ Prev ], Dst ] = {
    new ColorTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : EdgeTransition[ Node6[ Prev ], Dst ] = {
    new EdgeTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : NodeTransition[ Node6[ Prev ], Dst ] = {
    new NodeTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : StyleTransition[ Node6[ Prev ], Dst ] = {
    new StyleTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6End1[ Prev, Dst ] ( implicit t : EndTransition[ Node12[ Node6[ Prev ] ], Dst ] ) : EndTransition[ Node6[ Prev ], Dst ] = {
    new EndTransition[ Node6[ Prev ], Dst ] {
      def transit( src : Node6[ Prev ] ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node12[ Node6[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode7And[ Prev ] : AndTransition[ Node7[ Prev ], Node4[ Node7[ Prev ] ] ] = {
    new AndTransition[ Node7[ Prev ], Node4[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Node4[ Node7[ Prev ] ] = {
        Node4[ Node7[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode7To1[ Prev, Dst ] ( implicit t : ToTransition[ Node18[ Node7[ Prev ] ], Dst ] ) : ToTransition[ Node7[ Prev ], Dst ] = {
    new ToTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = AndsNull(  )
        t.transit( Node18[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode8End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Node9[ Node1[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node8[ Node9[ Node1[ Prev ] ] ], Dst ] {
      def transit( src : Node8[ Node9[ Node1[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = Directed( src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode9Edge[ Prev ] : EdgeTransition[ Node9[ Prev ], Node7[ Node9[ Prev ] ] ] = {
    new EdgeTransition[ Node9[ Prev ], Node7[ Node9[ Prev ] ] ] {
      def transit( src : Node9[ Prev ], arg1 : String ) : Node7[ Node9[ Prev ] ] = {
        Node7[ Node9[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode9Node[ Prev ] : NodeTransition[ Node9[ Prev ], Node5[ Node9[ Prev ] ] ] = {
    new NodeTransition[ Node9[ Prev ], Node5[ Node9[ Prev ] ] ] {
      def transit( src : Node9[ Prev ], arg1 : String ) : Node5[ Node9[ Prev ] ] = {
        Node5[ Node9[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode9End1[ Prev, Dst ] ( implicit t : EndTransition[ Node8[ Node9[ Prev ] ], Dst ] ) : EndTransition[ Node9[ Prev ], Dst ] = {
    new EndTransition[ Node9[ Prev ], Dst ] {
      def transit( src : Node9[ Prev ] ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node8[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode10Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : ColorTransition[ Node10[ Node11[ Prev ] ], Dst ] = {
    new ColorTransition[ Node10[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode10Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : ColorTransition[ Node10[ Node12[ Prev ] ], Dst ] = {
    new ColorTransition[ Node10[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode10Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node10[ Node11[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node10[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode10Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : EdgeTransition[ Node10[ Node12[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node10[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode10Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node10[ Node11[ Prev ] ], Dst ] = {
    new NodeTransition[ Node10[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode10Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : NodeTransition[ Node10[ Node12[ Prev ] ], Dst ] = {
    new NodeTransition[ Node10[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode10Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : StyleTransition[ Node10[ Node11[ Prev ] ], Dst ] = {
    new StyleTransition[ Node10[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode10Style2[ Prev, Dst ] ( implicit t : StyleTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : StyleTransition[ Node10[ Node12[ Prev ] ], Dst ] = {
    new StyleTransition[ Node10[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode10End1[ Prev, Dst ] ( implicit t : EndTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node11[ Prev ] ], Dst ] = {
    new EndTransition[ Node10[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node11[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode10End2[ Prev, Dst ] ( implicit t : EndTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node12[ Prev ] ], Dst ] = {
    new EndTransition[ Node10[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node10[ Node12[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrColor( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode11Color[ Prev ] : ColorTransition[ Node11[ Prev ], Node10[ Node11[ Prev ] ] ] = {
    new ColorTransition[ Node11[ Prev ], Node10[ Node11[ Prev ] ] ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Node10[ Node11[ Prev ] ] = {
        Node10[ Node11[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode11Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node11[ Prev ], Dst ] = {
    new EdgeTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode11Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node11[ Prev ], Dst ] = {
    new NodeTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode11Style[ Prev ] : StyleTransition[ Node11[ Prev ], Node13[ Node11[ Prev ] ] ] = {
    new StyleTransition[ Node11[ Prev ], Node13[ Node11[ Prev ] ] ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Node13[ Node11[ Prev ] ] = {
        Node13[ Node11[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode11End1[ Prev, Dst ] ( implicit t : EndTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node11[ Prev ], Dst ] = {
    new EndTransition[ Node11[ Prev ], Dst ] {
      def transit( src : Node11[ Prev ] ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode12Color[ Prev ] : ColorTransition[ Node12[ Prev ], Node10[ Node12[ Prev ] ] ] = {
    new ColorTransition[ Node12[ Prev ], Node10[ Node12[ Prev ] ] ] {
      def transit( src : Node12[ Prev ], arg1 : String ) : Node10[ Node12[ Prev ] ] = {
        Node10[ Node12[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode12Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EdgeTransition[ Node12[ Prev ], Dst ] = {
    new EdgeTransition[ Node12[ Prev ], Dst ] {
      def transit( src : Node12[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode12Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : NodeTransition[ Node12[ Prev ], Dst ] = {
    new NodeTransition[ Node12[ Prev ], Dst ] {
      def transit( src : Node12[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode12Style[ Prev ] : StyleTransition[ Node12[ Prev ], Node13[ Node12[ Prev ] ] ] = {
    new StyleTransition[ Node12[ Prev ], Node13[ Node12[ Prev ] ] ] {
      def transit( src : Node12[ Prev ], arg1 : String ) : Node13[ Node12[ Prev ] ] = {
        Node13[ Node12[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode12End1[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node12[ Prev ], Dst ] = {
    new EndTransition[ Node12[ Prev ], Dst ] {
      def transit( src : Node12[ Prev ] ) : Dst = {
        val prev = src
        val tree = EdgeAttrsNull(  )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode13Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : ColorTransition[ Node13[ Node11[ Prev ] ], Dst ] = {
    new ColorTransition[ Node13[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : ColorTransition[ Node13[ Node12[ Prev ] ], Dst ] = {
    new ColorTransition[ Node13[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node13[ Node11[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node13[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : EdgeTransition[ Node13[ Node12[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node13[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node13[ Node11[ Prev ] ], Dst ] = {
    new NodeTransition[ Node13[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : NodeTransition[ Node13[ Node12[ Prev ] ], Dst ] = {
    new NodeTransition[ Node13[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13Style1[ Prev, Dst ] ( implicit t : StyleTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : StyleTransition[ Node13[ Node11[ Prev ] ], Dst ] = {
    new StyleTransition[ Node13[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node11[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode13Style2[ Prev, Dst ] ( implicit t : StyleTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : StyleTransition[ Node13[ Node12[ Prev ] ], Dst ] = {
    new StyleTransition[ Node13[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node12[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13End1[ Prev, Dst ] ( implicit t : EndTransition[ Node11[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node13[ Node11[ Prev ] ], Dst ] = {
    new EndTransition[ Node13[ Node11[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node11[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode13End2[ Prev, Dst ] ( implicit t : EndTransition[ Node11[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node13[ Node12[ Prev ] ], Dst ] = {
    new EndTransition[ Node13[ Node12[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node12[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = EdgeAttrStyle( src.arg1 )
        t.transit( Node11[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode14Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : EdgeTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node11[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode14Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EdgeTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node12[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : NodeTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node11[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode14Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : NodeTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node12[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14End1[ Prev, Dst ] ( implicit t : EndTransition[ Node14[ Node11[ Prev ] ], Dst ] ) : EndTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node14[ Node11[ Node11[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node11[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node14[ Node11[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode14End2[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node14[ Node11[ Node12[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node11[ Node12[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = EdgeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode15Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] = {
    new EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode15Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode15Edge3[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] = {
    new EdgeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] = {
    new NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode15Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode15Node3[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] = {
    new NodeTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode15End1[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node9[ Prev ] ] ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode15End2[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node16[ Prev ] ] ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode15End3[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node6[ Node18[ Node7[ Node17[ Prev ] ] ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev.prev.prev
        val tree = EdgeStmt( src.prev.prev.prev.prev.arg1, src.prev.prev.prev.arg1, src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode16Edge[ Prev ] : EdgeTransition[ Node16[ Prev ], Node7[ Node16[ Prev ] ] ] = {
    new EdgeTransition[ Node16[ Prev ], Node7[ Node16[ Prev ] ] ] {
      def transit( src : Node16[ Prev ], arg1 : String ) : Node7[ Node16[ Prev ] ] = {
        Node7[ Node16[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode16Node[ Prev ] : NodeTransition[ Node16[ Prev ], Node5[ Node16[ Prev ] ] ] = {
    new NodeTransition[ Node16[ Prev ], Node5[ Node16[ Prev ] ] ] {
      def transit( src : Node16[ Prev ], arg1 : String ) : Node5[ Node16[ Prev ] ] = {
        Node5[ Node16[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode16End1[ Prev, Dst ] ( implicit t : EndTransition[ Node25[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node16[ Prev ], Dst ] = {
    new EndTransition[ Node16[ Prev ], Dst ] {
      def transit( src : Node16[ Prev ] ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node25[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode17Edge[ Prev ] : EdgeTransition[ Node17[ Prev ], Node7[ Node17[ Prev ] ] ] = {
    new EdgeTransition[ Node17[ Prev ], Node7[ Node17[ Prev ] ] ] {
      def transit( src : Node17[ Prev ], arg1 : String ) : Node7[ Node17[ Prev ] ] = {
        Node7[ Node17[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode17Node[ Prev ] : NodeTransition[ Node17[ Prev ], Node5[ Node17[ Prev ] ] ] = {
    new NodeTransition[ Node17[ Prev ], Node5[ Node17[ Prev ] ] ] {
      def transit( src : Node17[ Prev ], arg1 : String ) : Node5[ Node17[ Prev ] ] = {
        Node5[ Node17[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode17End1[ Prev, Dst ] ( implicit t : EndTransition[ Node26[ Node17[ Prev ] ], Dst ] ) : EndTransition[ Node17[ Prev ], Dst ] = {
    new EndTransition[ Node17[ Prev ], Dst ] {
      def transit( src : Node17[ Prev ] ) : Dst = {
        val prev = src
        val tree = StmtsNull(  )
        t.transit( Node26[ Node17[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode18To[ Prev ] : ToTransition[ Node18[ Prev ], Node6[ Node18[ Prev ] ] ] = {
    new ToTransition[ Node18[ Prev ], Node6[ Node18[ Prev ] ] ] {
      def transit( src : Node18[ Prev ], arg1 : String ) : Node6[ Node18[ Prev ] ] = {
        Node6[ Node18[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode19Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : ColorTransition[ Node19[ Node20[ Prev ] ], Dst ] = {
    new ColorTransition[ Node19[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode19Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : ColorTransition[ Node19[ Node21[ Prev ] ], Dst ] = {
    new ColorTransition[ Node19[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : EdgeTransition[ Node19[ Node20[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node19[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode19Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : EdgeTransition[ Node19[ Node21[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node19[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : NodeTransition[ Node19[ Node20[ Prev ] ], Dst ] = {
    new NodeTransition[ Node19[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode19Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : NodeTransition[ Node19[ Node21[ Prev ] ], Dst ] = {
    new NodeTransition[ Node19[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : ShapeTransition[ Node19[ Node20[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node19[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode19Shape2[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : ShapeTransition[ Node19[ Node21[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node19[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode19End1[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : EndTransition[ Node19[ Node20[ Prev ] ], Dst ] = {
    new EndTransition[ Node19[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node20[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode19End2[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : EndTransition[ Node19[ Node21[ Prev ] ], Dst ] = {
    new EndTransition[ Node19[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node19[ Node21[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrColor( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode20Color[ Prev ] : ColorTransition[ Node20[ Prev ], Node19[ Node20[ Prev ] ] ] = {
    new ColorTransition[ Node20[ Prev ], Node19[ Node20[ Prev ] ] ] {
      def transit( src : Node20[ Prev ], arg1 : String ) : Node19[ Node20[ Prev ] ] = {
        Node19[ Node20[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode20Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : EdgeTransition[ Node20[ Prev ], Dst ] = {
    new EdgeTransition[ Node20[ Prev ], Dst ] {
      def transit( src : Node20[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode20Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : NodeTransition[ Node20[ Prev ], Dst ] = {
    new NodeTransition[ Node20[ Prev ], Dst ] {
      def transit( src : Node20[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode20Shape[ Prev ] : ShapeTransition[ Node20[ Prev ], Node22[ Node20[ Prev ] ] ] = {
    new ShapeTransition[ Node20[ Prev ], Node22[ Node20[ Prev ] ] ] {
      def transit( src : Node20[ Prev ], arg1 : String ) : Node22[ Node20[ Prev ] ] = {
        Node22[ Node20[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode20End1[ Prev, Dst ] ( implicit t : EndTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : EndTransition[ Node20[ Prev ], Dst ] = {
    new EndTransition[ Node20[ Prev ], Dst ] {
      def transit( src : Node20[ Prev ] ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode21Color[ Prev ] : ColorTransition[ Node21[ Prev ], Node19[ Node21[ Prev ] ] ] = {
    new ColorTransition[ Node21[ Prev ], Node19[ Node21[ Prev ] ] ] {
      def transit( src : Node21[ Prev ], arg1 : String ) : Node19[ Node21[ Prev ] ] = {
        Node19[ Node21[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode21Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : EdgeTransition[ Node21[ Prev ], Dst ] = {
    new EdgeTransition[ Node21[ Prev ], Dst ] {
      def transit( src : Node21[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode21Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : NodeTransition[ Node21[ Prev ], Dst ] = {
    new NodeTransition[ Node21[ Prev ], Dst ] {
      def transit( src : Node21[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def shiftNode21Shape[ Prev ] : ShapeTransition[ Node21[ Prev ], Node22[ Node21[ Prev ] ] ] = {
    new ShapeTransition[ Node21[ Prev ], Node22[ Node21[ Prev ] ] ] {
      def transit( src : Node21[ Prev ], arg1 : String ) : Node22[ Node21[ Prev ] ] = {
        Node22[ Node21[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode21End1[ Prev, Dst ] ( implicit t : EndTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : EndTransition[ Node21[ Prev ], Dst ] = {
    new EndTransition[ Node21[ Prev ], Dst ] {
      def transit( src : Node21[ Prev ] ) : Dst = {
        val prev = src
        val tree = NodeAttrsNull(  )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode22Color1[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : ColorTransition[ Node22[ Node20[ Prev ] ], Dst ] = {
    new ColorTransition[ Node22[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode22Color2[ Prev, Dst ] ( implicit t : ColorTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : ColorTransition[ Node22[ Node21[ Prev ] ], Dst ] = {
    new ColorTransition[ Node22[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : EdgeTransition[ Node22[ Node20[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node22[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode22Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : EdgeTransition[ Node22[ Node21[ Prev ] ], Dst ] = {
    new EdgeTransition[ Node22[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : NodeTransition[ Node22[ Node20[ Prev ] ], Dst ] = {
    new NodeTransition[ Node22[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode22Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : NodeTransition[ Node22[ Node21[ Prev ] ], Dst ] = {
    new NodeTransition[ Node22[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22Shape1[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : ShapeTransition[ Node22[ Node20[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node22[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node20[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode22Shape2[ Prev, Dst ] ( implicit t : ShapeTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : ShapeTransition[ Node22[ Node21[ Prev ] ], Dst ] = {
    new ShapeTransition[ Node22[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node21[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode22End1[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node20[ Prev ] ], Dst ] ) : EndTransition[ Node22[ Node20[ Prev ] ], Dst ] = {
    new EndTransition[ Node22[ Node20[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node20[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node20[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode22End2[ Prev, Dst ] ( implicit t : EndTransition[ Node20[ Node21[ Prev ] ], Dst ] ) : EndTransition[ Node22[ Node21[ Prev ] ], Dst ] = {
    new EndTransition[ Node22[ Node21[ Prev ] ], Dst ] {
      def transit( src : Node22[ Node21[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = NodeAttrShape( src.arg1 )
        t.transit( Node20[ Node21[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode23Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : EdgeTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node20[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode23Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : EdgeTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] = {
    new EdgeTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node21[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode23Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : NodeTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node20[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode23Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : NodeTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] = {
    new NodeTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node21[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode23End1[ Prev, Dst ] ( implicit t : EndTransition[ Node23[ Node20[ Prev ] ], Dst ] ) : EndTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node23[ Node20[ Node20[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node20[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node23[ Node20[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode23End2[ Prev, Dst ] ( implicit t : EndTransition[ Node24[ Node21[ Prev ] ], Dst ] ) : EndTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node23[ Node20[ Node21[ Prev ] ] ], Dst ] {
      def transit( src : Node23[ Node20[ Node21[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = NodeAttrsCons( src.prev.arg1, src.arg1 )
        t.transit( Node24[ Node21[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode24Edge1[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : EdgeTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] = {
    new EdgeTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode24Edge2[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : EdgeTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] = {
    new EdgeTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode24Edge3[ Prev, Dst ] ( implicit t : EdgeTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : EdgeTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] = {
    new EdgeTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode24Node1[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : NodeTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] = {
    new NodeTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode24Node2[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : NodeTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] = {
    new NodeTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode24Node3[ Prev, Dst ] ( implicit t : NodeTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : NodeTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] = {
    new NodeTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode24End1[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node9[ Prev ] ], Dst ] ) : EndTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node9[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode24End2[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node16[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode24End3[ Prev, Dst ] ( implicit t : EndTransition[ Node16[ Node17[ Prev ] ], Dst ] ) : EndTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ], Dst ] {
      def transit( src : Node24[ Node21[ Node5[ Node17[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = NodeStmt( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node16[ Node17[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode25End1[ Prev, Dst ] ( implicit t : EndTransition[ Node8[ Node9[ Prev ] ], Dst ] ) : EndTransition[ Node25[ Node16[ Node9[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node25[ Node16[ Node9[ Prev ] ] ], Dst ] {
      def transit( src : Node25[ Node16[ Node9[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node8[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode25End2[ Prev, Dst ] ( implicit t : EndTransition[ Node25[ Node16[ Prev ] ], Dst ] ) : EndTransition[ Node25[ Node16[ Node16[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node25[ Node16[ Node16[ Prev ] ] ], Dst ] {
      def transit( src : Node25[ Node16[ Node16[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node25[ Node16[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode25End3[ Prev, Dst ] ( implicit t : EndTransition[ Node26[ Node17[ Prev ] ], Dst ] ) : EndTransition[ Node25[ Node16[ Node17[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node25[ Node16[ Node17[ Prev ] ] ], Dst ] {
      def transit( src : Node25[ Node16[ Node17[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = StmtsCons( src.prev.arg1, src.arg1 )
        t.transit( Node26[ Node17[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode26End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node26[ Node17[ Node1[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node26[ Node17[ Node1[ Prev ] ] ], Dst ] {
      def transit( src : Node26[ Node17[ Node1[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = Undirected( src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def begin() : Node1[ Unit ] = Node1( () )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

