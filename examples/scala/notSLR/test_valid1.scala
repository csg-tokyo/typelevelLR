
object test_valid1 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().a().g().d().end()
    println( tree )
  }
}
