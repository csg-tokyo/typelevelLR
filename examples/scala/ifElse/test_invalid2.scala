
object test_invalid2 {
  import ifElse._
  def main(args : Array[String]) = {
    println( begin().if_(true).return_("A").end() )
  }
}

