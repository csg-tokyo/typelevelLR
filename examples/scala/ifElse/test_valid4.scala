
object test_valid4 {
  import ifElse._
  def main(args : Array[String]) = {
    println( begin().if_(true).then_().if_(true).then_().return_("A").else_().return_("B").else_().return_("C").end() )
  }
}

