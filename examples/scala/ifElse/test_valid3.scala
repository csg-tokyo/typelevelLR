
object test_valid3 {
  import ifElse._
  def main(args : Array[String]) = {
    println( begin().if_(true).then_().return_("A").end() )
  }
}

