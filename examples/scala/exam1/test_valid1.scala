
object test_valid1 {

  import exam1._

  val graph = begin()
    .digraph("small_graph")
      .node("A").shape("rectangle")
      .node("B").node("C").shape("doublecircle")
      .edge("A").to("B").style("dotted")
      .edge("A").and("B").to("C")
    .end()

  def main( args : Array[ String ] ) = {
    println( graph )
  }

}
