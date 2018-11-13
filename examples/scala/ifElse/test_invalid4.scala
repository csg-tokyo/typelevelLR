
object test_invalid4 {
  import ifElse._
  def main(args : Array[String]) = {
    println( begin().if_(true).then_().if_(false).then_().return_("A").else_().return_("B").else_().return_("C").else_().return_("D").end() )
  }
}

