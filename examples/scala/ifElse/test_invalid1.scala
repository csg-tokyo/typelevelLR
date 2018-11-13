
object test_invalid1 {
  import ifElse._
  def main(args : Array[String]) = {
    println( begin().if_(true).end() )
  }
}

