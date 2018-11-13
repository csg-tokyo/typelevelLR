
object test_valid3 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().b().g().d().end()
    println( tree )
  }
}
