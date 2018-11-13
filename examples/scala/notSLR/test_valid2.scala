
object test_valid2 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().a().g().c().end()
    println( tree )
  }
}
