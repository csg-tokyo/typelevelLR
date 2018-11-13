
object test_valid4 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().b().g().c().end()
    println( tree )
  }
}
