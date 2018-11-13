
object test_invalid2 {
  import notSLR._

  def main(args : Array[String]) = {
    val tree : S = begin().a().b().c()end()
    println( tree )
  }
}
