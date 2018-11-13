
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar

// Return : Expr -> "return_(String)"
// IfThen : Expr -> "if_(Boolean)" Then
// IfThenElse : Expr -> "if_(Boolean)" Then Else
// ElseClause : Else -> "else_()" Expr
// ThenClause : Then -> "then_()" Expr

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object ifElse {

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // AST

  trait Expr
  case class Return ( arg1 : String ) extends Expr
  case class IfThen ( arg1 : Boolean, arg2 : Then ) extends Expr
  case class IfThenElse ( arg1 : Boolean, arg2 : Then, arg3 : Else ) extends Expr

  trait Else
  case class ElseClause ( arg1 : Expr ) extends Else

  trait Then
  case class ThenClause ( arg1 : Expr ) extends Then

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition type classes

  trait Else_Transition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait Return_Transition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait If_Transition [ Src, Dst ] {
    def transit( src : Src, arg1 : Boolean ) : Dst
  }

  trait Then_Transition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait EndTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }


  // implicit classes for transition methods

  implicit class Else_Transitable [ Src, Dst ] ( src : Src ) ( implicit t : Else_Transition[ Src, Dst ] ) {
    def else_(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class Return_Transitable [ Src, Dst ] ( src : Src ) ( implicit t : Return_Transition[ Src, Dst ] ) {
    def return_( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class If_Transitable [ Src, Dst ] ( src : Src ) ( implicit t : If_Transition[ Src, Dst ] ) {
    def if_( arg1 : Boolean ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class Then_Transitable [ Src, Dst ] ( src : Src ) ( implicit t : Then_Transition[ Src, Dst ] ) {
    def then_(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class EndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EndTransition[ Src, Dst ] ) {
    def end() : Dst = t.transit( src )
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // stack elements

  case class Node1 [ Prev ] ( prev : Prev )

  case class Node2 [ Prev ] ( prev : Prev, arg1 : Expr )

  case class Node3 [ Prev ] ( prev : Prev, arg1 : Expr )

  case class Node4 [ Prev ] ( prev : Prev )

  case class Node5 [ Prev ] ( prev : Prev, arg1 : Then )

  case class Node6 [ Prev ] ( prev : Prev, arg1 : Boolean )

  case class Node7 [ Prev ] ( prev : Prev )

  case class Node8 [ Prev ] ( prev : Prev, arg1 : Else )

  case class Node9 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node10 [ Prev ] ( prev : Prev, arg1 : Expr )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition implementations

  implicit def acceptNode2[ Prev ] : EndTransition[ Node2[ Prev ], Expr ] = {
    new EndTransition[ Node2[ Prev ], Expr ] {
      def transit( src : Node2[ Prev ] ) : Expr = {
        src.arg1
      }
    }
  }

  implicit def shiftNode1If_[ Prev ] : If_Transition[ Node1[ Prev ], Node6[ Node1[ Prev ] ] ] = {
    new If_Transition[ Node1[ Prev ], Node6[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : Boolean ) : Node6[ Node1[ Prev ] ] = {
        Node6[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode1Return_[ Prev ] : Return_Transition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] = {
    new Return_Transition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node9[ Node1[ Prev ] ] = {
        Node9[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode3Else_1[ Prev, Dst ] ( implicit t : Else_Transition[ Node8[ Node5[ Prev ] ], Dst ] ) : Else_Transition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new Else_Transition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = ElseClause( src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode3End1[ Prev, Dst ] ( implicit t : EndTransition[ Node8[ Node5[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node5[ Prev ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = ElseClause( src.arg1 )
        t.transit( Node8[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode4If_[ Prev ] : If_Transition[ Node4[ Prev ], Node6[ Node4[ Prev ] ] ] = {
    new If_Transition[ Node4[ Prev ], Node6[ Node4[ Prev ] ] ] {
      def transit( src : Node4[ Prev ], arg1 : Boolean ) : Node6[ Node4[ Prev ] ] = {
        Node6[ Node4[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode4Return_[ Prev ] : Return_Transition[ Node4[ Prev ], Node9[ Node4[ Prev ] ] ] = {
    new Return_Transition[ Node4[ Prev ], Node9[ Node4[ Prev ] ] ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Node9[ Node4[ Prev ] ] = {
        Node9[ Node4[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode5Else_[ Prev ] : Else_Transition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] = {
    new Else_Transition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] {
      def transit( src : Node5[ Prev ] ) : Node4[ Node5[ Prev ] ] = {
        Node4[ Node5[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode5End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node5[ Node6[ Node1[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node5[ Node6[ Node1[ Prev ] ] ], Dst ] {
      def transit( src : Node5[ Node6[ Node1[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = IfThen( src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode5End2[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node5[ Node6[ Node4[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node5[ Node6[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node5[ Node6[ Node4[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = IfThen( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode5End3[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node5[ Node6[ Node7[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node5[ Node6[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node5[ Node6[ Node7[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = IfThen( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode6Then_[ Prev ] : Then_Transition[ Node6[ Prev ], Node7[ Node6[ Prev ] ] ] = {
    new Then_Transition[ Node6[ Prev ], Node7[ Node6[ Prev ] ] ] {
      def transit( src : Node6[ Prev ] ) : Node7[ Node6[ Prev ] ] = {
        Node7[ Node6[ Prev ] ]( src )
      }
    }
  }

  implicit def shiftNode7If_[ Prev ] : If_Transition[ Node7[ Prev ], Node6[ Node7[ Prev ] ] ] = {
    new If_Transition[ Node7[ Prev ], Node6[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ], arg1 : Boolean ) : Node6[ Node7[ Prev ] ] = {
        Node6[ Node7[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode7Return_[ Prev ] : Return_Transition[ Node7[ Prev ], Node9[ Node7[ Prev ] ] ] = {
    new Return_Transition[ Node7[ Prev ], Node9[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Node9[ Node7[ Prev ] ] = {
        Node9[ Node7[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode8Else_1[ Prev, Dst ] ( implicit t : Else_Transition[ Node2[ Node1[ Prev ] ], Dst ] ) : Else_Transition[ Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ], Dst ] = {
    new Else_Transition[ Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode8Else_2[ Prev, Dst ] ( implicit t : Else_Transition[ Node3[ Node4[ Prev ] ], Dst ] ) : Else_Transition[ Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ], Dst ] = {
    new Else_Transition[ Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode8Else_3[ Prev, Dst ] ( implicit t : Else_Transition[ Node10[ Node7[ Prev ] ], Dst ] ) : Else_Transition[ Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ], Dst ] = {
    new Else_Transition[ Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode8End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode8End2[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node4[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode8End3[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node5[ Node6[ Node7[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = IfThenElse( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode9Else_1[ Prev, Dst ] ( implicit t : Else_Transition[ Node2[ Node1[ Prev ] ], Dst ] ) : Else_Transition[ Node9[ Node1[ Prev ] ], Dst ] = {
    new Else_Transition[ Node9[ Node1[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node1[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode9Else_2[ Prev, Dst ] ( implicit t : Else_Transition[ Node3[ Node4[ Prev ] ], Dst ] ) : Else_Transition[ Node9[ Node4[ Prev ] ], Dst ] = {
    new Else_Transition[ Node9[ Node4[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node4[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode9Else_3[ Prev, Dst ] ( implicit t : Else_Transition[ Node10[ Node7[ Prev ] ], Dst ] ) : Else_Transition[ Node9[ Node7[ Prev ] ], Dst ] = {
    new Else_Transition[ Node9[ Node7[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node7[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node10[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode9End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node9[ Node1[ Prev ] ], Dst ] = {
    new EndTransition[ Node9[ Node1[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node1[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode9End2[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node9[ Node4[ Prev ] ], Dst ] = {
    new EndTransition[ Node9[ Node4[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node4[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode9End3[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node9[ Node7[ Prev ] ], Dst ] = {
    new EndTransition[ Node9[ Node7[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node7[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Return( src.arg1 )
        t.transit( Node10[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode10Else_1[ Prev, Dst ] ( implicit t : Else_Transition[ Node5[ Node6[ Prev ] ], Dst ] ) : Else_Transition[ Node10[ Node7[ Node6[ Prev ] ] ], Dst ] = {
    new Else_Transition[ Node10[ Node7[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node10[ Node7[ Node6[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = ThenClause( src.arg1 )
        t.transit( Node5[ Node6[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode10End1[ Prev, Dst ] ( implicit t : EndTransition[ Node5[ Node6[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node7[ Node6[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node10[ Node7[ Node6[ Prev ] ] ], Dst ] {
      def transit( src : Node10[ Node7[ Node6[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = ThenClause( src.arg1 )
        t.transit( Node5[ Node6[ Prev ] ]( prev, tree ) )
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def begin() : Node1[ Unit ] = Node1( () )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

