
object test_invalid1 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().a().g().end()
    println( tree )
  }
}
