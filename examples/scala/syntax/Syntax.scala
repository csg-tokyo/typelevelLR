
import scala.language.implicitConversions

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar

// DefineSyntax : Syntax -> "syntax(String)" "startsWith(String)" Rules
// RuleDerive : Rule -> "rule(String)" "derive(String)" RuleBody
// RuleBodyTo : RuleBody -> "to(String)" RuleTail
// RuleBodyToEpsilon : RuleBody -> "toEpsilon()"
// RuleTailTo : RuleTail -> "andThen(String)" RuleTail
// RuleTailEpsilon : RuleTail -> eps
// RulesCons : Rules -> Rule Rules
// RulesNull : Rules -> eps

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

object Syntax {

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // AST

  trait Syntax
  case class DefineSyntax ( arg1 : String, arg2 : String, arg3 : Rules ) extends Syntax

  trait Rule
  case class RuleDerive ( arg1 : String, arg2 : String, arg3 : RuleBody ) extends Rule

  trait RuleBody
  case class RuleBodyTo ( arg1 : String, arg2 : RuleTail ) extends RuleBody
  case class RuleBodyToEpsilon (  ) extends RuleBody

  trait RuleTail
  case class RuleTailTo ( arg1 : String, arg2 : RuleTail ) extends RuleTail
  case class RuleTailEpsilon (  ) extends RuleTail

  trait Rules
  case class RulesCons ( arg1 : Rule, arg2 : Rules ) extends Rules
  case class RulesNull (  ) extends Rules

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition type classes

  trait RuleTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait DeriveTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait ToTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait ToEpsilonTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }

  trait AndThenTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait SyntaxTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait StartsWithTransition [ Src, Dst ] {
    def transit( src : Src, arg1 : String ) : Dst
  }

  trait EndTransition [ Src, Dst ] {
    def transit( src : Src ) : Dst
  }


  // implicit classes for transition methods

  implicit class RuleTransitable [ Src, Dst ] ( src : Src ) ( implicit t : RuleTransition[ Src, Dst ] ) {
    def rule( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class DeriveTransitable [ Src, Dst ] ( src : Src ) ( implicit t : DeriveTransition[ Src, Dst ] ) {
    def derive( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class ToTransitable [ Src, Dst ] ( src : Src ) ( implicit t : ToTransition[ Src, Dst ] ) {
    def to( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class ToEpsilonTransitable [ Src, Dst ] ( src : Src ) ( implicit t : ToEpsilonTransition[ Src, Dst ] ) {
    def toEpsilon(  ) : Dst = {
      t.transit( src )
    }
  }

  implicit class AndThenTransitable [ Src, Dst ] ( src : Src ) ( implicit t : AndThenTransition[ Src, Dst ] ) {
    def andThen( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class SyntaxTransitable [ Src, Dst ] ( src : Src ) ( implicit t : SyntaxTransition[ Src, Dst ] ) {
    def syntax( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class StartsWithTransitable [ Src, Dst ] ( src : Src ) ( implicit t : StartsWithTransition[ Src, Dst ] ) {
    def startsWith( arg1 : String ) : Dst = {
      t.transit( src, arg1 )
    }
  }

  implicit class EndTransitable [ Src, Dst ] ( src : Src ) ( implicit t : EndTransition[ Src, Dst ] ) {
    def end() : Dst = t.transit( src )
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // stack elements

  case class Node1 [ Prev ] ( prev : Prev )

  case class Node2 [ Prev ] ( prev : Prev, arg1 : Syntax )

  case class Node3 [ Prev ] ( prev : Prev, arg1 : Rules )

  case class Node4 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node5 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node6 [ Prev ] ( prev : Prev, arg1 : RuleTail )

  case class Node7 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node8 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node9 [ Prev ] ( prev : Prev )

  case class Node10 [ Prev ] ( prev : Prev, arg1 : RuleBody )

  case class Node11 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node12 [ Prev ] ( prev : Prev, arg1 : Rule )

  case class Node13 [ Prev ] ( prev : Prev, arg1 : String )

  case class Node14 [ Prev ] ( prev : Prev, arg1 : RuleTail )

  case class Node15 [ Prev ] ( prev : Prev, arg1 : Rules )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // transition implementations

  implicit def acceptNode2[ Prev ] : EndTransition[ Node2[ Prev ], Syntax ] = {
    new EndTransition[ Node2[ Prev ], Syntax ] {
      def transit( src : Node2[ Prev ] ) : Syntax = {
        src.arg1
      }
    }
  }

  implicit def shiftNode1Syntax[ Prev ] : SyntaxTransition[ Node1[ Prev ], Node5[ Node1[ Prev ] ] ] = {
    new SyntaxTransition[ Node1[ Prev ], Node5[ Node1[ Prev ] ] ] {
      def transit( src : Node1[ Prev ], arg1 : String ) : Node5[ Node1[ Prev ] ] = {
        Node5[ Node1[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode3End1[ Prev, Dst ] ( implicit t : EndTransition[ Node2[ Node1[ Prev ] ], Dst ] ) : EndTransition[ Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ], Dst ] {
      def transit( src : Node3[ Node4[ Node5[ Node1[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = DefineSyntax( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node2[ Node1[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode4Rule[ Prev ] : RuleTransition[ Node4[ Prev ], Node11[ Node4[ Prev ] ] ] = {
    new RuleTransition[ Node4[ Prev ], Node11[ Node4[ Prev ] ] ] {
      def transit( src : Node4[ Prev ], arg1 : String ) : Node11[ Node4[ Prev ] ] = {
        Node11[ Node4[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode4End1[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node4[ Prev ], Dst ] = {
    new EndTransition[ Node4[ Prev ], Dst ] {
      def transit( src : Node4[ Prev ] ) : Dst = {
        val prev = src
        val tree = RulesNull(  )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode5StartsWith[ Prev ] : StartsWithTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] = {
    new StartsWithTransition[ Node5[ Prev ], Node4[ Node5[ Prev ] ] ] {
      def transit( src : Node5[ Prev ], arg1 : String ) : Node4[ Node5[ Prev ] ] = {
        Node4[ Node5[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode6Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node10[ Node8[ Prev ] ], Dst ] ) : RuleTransition[ Node6[ Node7[ Node8[ Prev ] ] ], Dst ] = {
    new RuleTransition[ Node6[ Node7[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node6[ Node7[ Node8[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleBodyTo( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode6End1[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node8[ Prev ] ], Dst ] ) : EndTransition[ Node6[ Node7[ Node8[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node6[ Node7[ Node8[ Prev ] ] ], Dst ] {
      def transit( src : Node6[ Node7[ Node8[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleBodyTo( src.prev.arg1, src.arg1 )
        t.transit( Node10[ Node8[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode7AndThen[ Prev ] : AndThenTransition[ Node7[ Prev ], Node13[ Node7[ Prev ] ] ] = {
    new AndThenTransition[ Node7[ Prev ], Node13[ Node7[ Prev ] ] ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Node13[ Node7[ Prev ] ] = {
        Node13[ Node7[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode7Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node6[ Node7[ Prev ] ], Dst ] ) : RuleTransition[ Node7[ Prev ], Dst ] = {
    new RuleTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = RuleTailEpsilon(  )
        t.transit( Node6[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode7End1[ Prev, Dst ] ( implicit t : EndTransition[ Node6[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node7[ Prev ], Dst ] = {
    new EndTransition[ Node7[ Prev ], Dst ] {
      def transit( src : Node7[ Prev ] ) : Dst = {
        val prev = src
        val tree = RuleTailEpsilon(  )
        t.transit( Node6[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode8To[ Prev ] : ToTransition[ Node8[ Prev ], Node7[ Node8[ Prev ] ] ] = {
    new ToTransition[ Node8[ Prev ], Node7[ Node8[ Prev ] ] ] {
      def transit( src : Node8[ Prev ], arg1 : String ) : Node7[ Node8[ Prev ] ] = {
        Node7[ Node8[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode8ToEpsilon[ Prev ] : ToEpsilonTransition[ Node8[ Prev ], Node9[ Node8[ Prev ] ] ] = {
    new ToEpsilonTransition[ Node8[ Prev ], Node9[ Node8[ Prev ] ] ] {
      def transit( src : Node8[ Prev ] ) : Node9[ Node8[ Prev ] ] = {
        Node9[ Node8[ Prev ] ]( src )
      }
    }
  }

  implicit def reduceNode9Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node10[ Node8[ Prev ] ], Dst ] ) : RuleTransition[ Node9[ Node8[ Prev ] ], Dst ] = {
    new RuleTransition[ Node9[ Node8[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node8[ Prev ] ], arg1 : String ) : Dst = {
        val prev = src.prev
        val tree = RuleBodyToEpsilon(  )
        t.transit( Node10[ Node8[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode9End1[ Prev, Dst ] ( implicit t : EndTransition[ Node10[ Node8[ Prev ] ], Dst ] ) : EndTransition[ Node9[ Node8[ Prev ] ], Dst ] = {
    new EndTransition[ Node9[ Node8[ Prev ] ], Dst ] {
      def transit( src : Node9[ Node8[ Prev ] ] ) : Dst = {
        val prev = src.prev
        val tree = RuleBodyToEpsilon(  )
        t.transit( Node10[ Node8[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode10Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node12[ Node4[ Prev ] ], Dst ] ) : RuleTransition[ Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ], Dst ] = {
    new RuleTransition[ Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ], Dst ] {
      def transit( src : Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = RuleDerive( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node4[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode10Rule2[ Prev, Dst ] ( implicit t : RuleTransition[ Node12[ Node12[ Prev ] ], Dst ] ) : RuleTransition[ Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ], Dst ] = {
    new RuleTransition[ Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ], Dst ] {
      def transit( src : Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = RuleDerive( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node12[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode10End1[ Prev, Dst ] ( implicit t : EndTransition[ Node12[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ], Dst ] {
      def transit( src : Node10[ Node8[ Node11[ Node4[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = RuleDerive( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode10End2[ Prev, Dst ] ( implicit t : EndTransition[ Node12[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ], Dst ] = {
    new EndTransition[ Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ], Dst ] {
      def transit( src : Node10[ Node8[ Node11[ Node12[ Prev ] ] ] ] ) : Dst = {
        val prev = src.prev.prev.prev
        val tree = RuleDerive( src.prev.prev.arg1, src.prev.arg1, src.arg1 )
        t.transit( Node12[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode11Derive[ Prev ] : DeriveTransition[ Node11[ Prev ], Node8[ Node11[ Prev ] ] ] = {
    new DeriveTransition[ Node11[ Prev ], Node8[ Node11[ Prev ] ] ] {
      def transit( src : Node11[ Prev ], arg1 : String ) : Node8[ Node11[ Prev ] ] = {
        Node8[ Node11[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def shiftNode12Rule[ Prev ] : RuleTransition[ Node12[ Prev ], Node11[ Node12[ Prev ] ] ] = {
    new RuleTransition[ Node12[ Prev ], Node11[ Node12[ Prev ] ] ] {
      def transit( src : Node12[ Prev ], arg1 : String ) : Node11[ Node12[ Prev ] ] = {
        Node11[ Node12[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode12End1[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node12[ Prev ], Dst ] = {
    new EndTransition[ Node12[ Prev ], Dst ] {
      def transit( src : Node12[ Prev ] ) : Dst = {
        val prev = src
        val tree = RulesNull(  )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def shiftNode13AndThen[ Prev ] : AndThenTransition[ Node13[ Prev ], Node13[ Node13[ Prev ] ] ] = {
    new AndThenTransition[ Node13[ Prev ], Node13[ Node13[ Prev ] ] ] {
      def transit( src : Node13[ Prev ], arg1 : String ) : Node13[ Node13[ Prev ] ] = {
        Node13[ Node13[ Prev ] ]( src, arg1 )
      }
    }
  }

  implicit def reduceNode13Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node14[ Node13[ Prev ] ], Dst ] ) : RuleTransition[ Node13[ Prev ], Dst ] = {
    new RuleTransition[ Node13[ Prev ], Dst ] {
      def transit( src : Node13[ Prev ], arg1 : String ) : Dst = {
        val prev = src
        val tree = RuleTailEpsilon(  )
        t.transit( Node14[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode13End1[ Prev, Dst ] ( implicit t : EndTransition[ Node14[ Node13[ Prev ] ], Dst ] ) : EndTransition[ Node13[ Prev ], Dst ] = {
    new EndTransition[ Node13[ Prev ], Dst ] {
      def transit( src : Node13[ Prev ] ) : Dst = {
        val prev = src
        val tree = RuleTailEpsilon(  )
        t.transit( Node14[ Node13[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode14Rule1[ Prev, Dst ] ( implicit t : RuleTransition[ Node6[ Node7[ Prev ] ], Dst ] ) : RuleTransition[ Node14[ Node13[ Node7[ Prev ] ] ], Dst ] = {
    new RuleTransition[ Node14[ Node13[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node13[ Node7[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleTailTo( src.prev.arg1, src.arg1 )
        t.transit( Node6[ Node7[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }
  implicit def reduceNode14Rule2[ Prev, Dst ] ( implicit t : RuleTransition[ Node14[ Node13[ Prev ] ], Dst ] ) : RuleTransition[ Node14[ Node13[ Node13[ Prev ] ] ], Dst ] = {
    new RuleTransition[ Node14[ Node13[ Node13[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node13[ Node13[ Prev ] ] ], arg1 : String ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleTailTo( src.prev.arg1, src.arg1 )
        t.transit( Node14[ Node13[ Prev ] ]( prev, tree ), arg1 )
      }
    }
  }

  implicit def reduceNode14End1[ Prev, Dst ] ( implicit t : EndTransition[ Node6[ Node7[ Prev ] ], Dst ] ) : EndTransition[ Node14[ Node13[ Node7[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node14[ Node13[ Node7[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node13[ Node7[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleTailTo( src.prev.arg1, src.arg1 )
        t.transit( Node6[ Node7[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode14End2[ Prev, Dst ] ( implicit t : EndTransition[ Node14[ Node13[ Prev ] ], Dst ] ) : EndTransition[ Node14[ Node13[ Node13[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node14[ Node13[ Node13[ Prev ] ] ], Dst ] {
      def transit( src : Node14[ Node13[ Node13[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = RuleTailTo( src.prev.arg1, src.arg1 )
        t.transit( Node14[ Node13[ Prev ] ]( prev, tree ) )
      }
    }
  }

  implicit def reduceNode15End1[ Prev, Dst ] ( implicit t : EndTransition[ Node3[ Node4[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node12[ Node4[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node12[ Node4[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node4[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = RulesCons( src.prev.arg1, src.arg1 )
        t.transit( Node3[ Node4[ Prev ] ]( prev, tree ) )
      }
    }
  }
  implicit def reduceNode15End2[ Prev, Dst ] ( implicit t : EndTransition[ Node15[ Node12[ Prev ] ], Dst ] ) : EndTransition[ Node15[ Node12[ Node12[ Prev ] ] ], Dst ] = {
    new EndTransition[ Node15[ Node12[ Node12[ Prev ] ] ], Dst ] {
      def transit( src : Node15[ Node12[ Node12[ Prev ] ] ] ) : Dst = {
        val prev = src.prev.prev
        val tree = RulesCons( src.prev.arg1, src.arg1 )
        t.transit( Node15[ Node12[ Prev ] ]( prev, tree ) )
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def begin() : Node1[ Unit ] = Node1( () )

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

