
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar

// Rule1 : S -> "a()" "g()" "d()"
// Rule2 : S -> "a()" A "c()"
// Rule3 : S -> "b()" A "d()"
// Rule4 : S -> "b()" "g()" "c()"
// Rule5 : A -> B
// Rule6 : B -> "g()"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object notSLR {

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // AST

  trait S
  case class Rule1 (  ) extends S
  case class Rule2 ( arg1 : A ) extends S
  case class Rule3 ( arg1 : A ) extends S
  case class Rule4 (  ) extends S

  trait A
  case class Rule5 ( arg1 : B ) extends A

  trait B
  case class Rule6 (  ) extends B

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition type classes

  trait GTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait ATransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait DTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait CTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait BTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait EndTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }


  // implicit classes for transition methods

  implicit class GTransitable [ Src, Dst ] ( src : Src ) ( implicit t : GTransition[ Src, Dst ] ) {
    def g(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class ATransitable [ Src, Dst ] ( src : Src ) ( implicit t : ATransition[ Src, Dst ] ) {
    def a(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class DTransitable [ Src, Dst ] ( src : Src ) ( implicit t : DTransition[ Src, Dst ] ) {
    def d(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class CTransitable [ Src, Dst ] ( src : Src ) ( implicit t : CTransition[ Src, Dst ] ) {
    def c(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class BTransitable [ Src, Dst ] ( src : Src ) ( implicit t : BTransition[ Src, Dst ] ) {
    def b(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class EndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EndTransition[ Src, Dst ] ) {
    def end() : Dst = t.transit( src )
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // stack elements

  case class Node1 [ Prev ] ( prev : Prev )

  case class Node2 [ Prev ] ( prev : Prev, arg1 : S )

  case class Node3 [ Prev ] ( prev : Prev )

  case class Node4 [ Prev ] ( prev : Prev )

  case class Node5 [ Prev ] ( prev : Prev )

  case class Node6 [ Prev ] ( prev : Prev )

  case class Node7 [ Prev ] ( prev : Prev, arg1 : A )

  case class Node8 [ Prev ] ( prev : Prev )

  case class Node9 [ Prev ] ( prev : Prev )

  case class Node10 [ Prev ] ( prev : Prev, arg1 : A )

  case class Node11 [ Prev ] ( prev : Prev )

  case class Node12 [ Prev ] ( prev : Prev )

  case class Node13 [ Prev ] ( prev : Prev, arg1 : B )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition implementations

  implicit def acceptNode2[ Prev ] : EndTransition[ Node2[ Prev ], S ] = {
    new EndTransition[ Node2[ Prev ], S ] {
      def transit( src : Node2[ Prev ] ) : S = {
        src.arg1
      }
    }
  }

  implicit def shiftNode1A[ Prev ] : ATransition[ Node1[ Prev ], Node5[ Node1[ Prev ] ] ] = {
    new ATransition[ Node1[ Prev ], Node5[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ] ) : Node5[ Node1[ Prev ] ] = {
        Node5[ Node1[ Prev ] ]( src )
      }
    }
  }

  implicit def shiftNode1B[ Prev ] : BTransition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] = {
    new BTransition[ Node1[ Prev ], Node9[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ] ) : Node9[ Node1[ Prev ] ] = {
        Node9[ Node1[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode3End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = Rule1(  )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode4C1[ Prev, Dst ] ( implicit t : CTransition[ Node13[ Node5[ Prev ] ], Dst ] ) : CTransition[ Node4[ Node5[ Prev ] ], Dst ] = {
    new CTransition[ Node4[ Node5[ Prev ] ], Dst ] {
      def transit( src : Node4[ Node5[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule6(  )
        t.transit( Node13[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode4D[ Prev ] : DTransition[ Node4[ Prev ], Node3[ Node4[ Prev ] ] ] = {
    new DTransition[ Node4[ Prev ], Node3[ Node4[ Prev ] ] ] {
      def transit( src : Node4[ Prev ] ) : Node3[ Node4[ Prev ] ] = {
        Node3[ Node4[ Prev ] ]( src )
      }
    }
  }

  implicit def shiftNode5G[ Prev ] : GTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] = {
    new GTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] {
      def transit( src : Node5[ Prev ] ) : Node4[ Node5[ Prev ] ] = {
        Node4[ Node5[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode6End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node6[ Node7[ Node5[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node6[ Node7[ Node5[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node6[ Node7[ Node5[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = Rule2( src.prev.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode7C[ Prev ] : CTransition[ Node7[ Prev ], Node6[ Node7[ Prev ] ] ] = {
    new CTransition[ Node7[ Prev ], Node6[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ] ) : Node6[ Node7[ Prev ] ] = {
        Node6[ Node7[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode8End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node8[ Node10[ Node9[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node8[ Node10[ Node9[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node8[ Node10[ Node9[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = Rule3( src.prev.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode9G[ Prev ] : GTransition[ Node9[ Prev ], Node12[ Node9[ Prev ] ] ] = {
    new GTransition[ Node9[ Prev ], Node12[ Node9[ Prev ] ] ] {
      def transit( src : Node9[ Prev ] ) : Node12[ Node9[ Prev ] ] = {
        Node12[ Node9[ Prev ] ]( src )
      }
    }
  }

  implicit def shiftNode10D[ Prev ] : DTransition[ Node10[ Prev ], Node8[ Node10[ Prev ] ] ] = {
    new DTransition[ Node10[ Prev ], Node8[ Node10[ Prev ] ] ] {
      def transit( src : Node10[ Prev ] ) : Node8[ Node10[ Prev ] ] = {
        Node8[ Node10[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode11End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node11[ Node12[ Node9[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node11[ Node12[ Node9[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node11[ Node12[ Node9[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = Rule4(  )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode12C[ Prev ] : CTransition[ Node12[ Prev ], Node11[ Node12[ Prev ] ] ] = {
    new CTransition[ Node12[ Prev ], Node11[ Node12[ Prev ] ] ] {
      def transit( src : Node12[ Prev ] ) : Node11[ Node12[ Prev ] ] = {
        Node11[ Node12[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode12D1[ Prev, Dst ] ( implicit t : DTransition[ Node13[ Node9[ Prev ] ], Dst ] ) : DTransition[ Node12[ Node9[ Prev ] ], Dst ] = {
    new DTransition[ Node12[ Node9[ Prev ] ], Dst ] {
      def transit( src : Node12[ Node9[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule6(  )
        t.transit( Node13[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode13C1[ Prev, Dst ] ( implicit t : CTransition[ Node7[ Node5[ Prev ] ], Dst ] ) : CTransition[ Node13[ Node5[ Prev ] ], Dst ] = {
    new CTransition[ Node13[ Node5[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node5[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule5( src.arg1 )
        t.transit( Node7[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode13C2[ Prev, Dst ] ( implicit t : CTransition[ Node10[ Node9[ Prev ] ], Dst ] ) : CTransition[ Node13[ Node9[ Prev ] ], Dst ] = {
    new CTransition[ Node13[ Node9[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node9[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule5( src.arg1 )
        t.transit( Node10[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode13D1[ Prev, Dst ] ( implicit t : DTransition[ Node7[ Node5[ Prev ] ], Dst ] ) : DTransition[ Node13[ Node5[ Prev ] ], Dst ] = {
    new DTransition[ Node13[ Node5[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node5[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule5( src.arg1 )
        t.transit( Node7[ Node5[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode13D2[ Prev, Dst ] ( implicit t : DTransition[ Node10[ Node9[ Prev ] ], Dst ] ) : DTransition[ Node13[ Node9[ Prev ] ], Dst ] = {
    new DTransition[ Node13[ Node9[ Prev ] ], Dst ] {
      def transit( src : Node13[ Node9[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = Rule5( src.arg1 )
        t.transit( Node10[ Node9[ Prev ] ]( prev, tree ) )
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def begin() : Node1[ Unit ] = Node1( () )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

