
// 17

object test {
  import exam1._

  def main(args : Array[String]) = {
    val graph = begin()
          .digraph("test")
.node("A")
.node("B")
.node("C")
.node("D")
.node("E")
.edge("A").to("B")
.edge("B").to("C")
.edge("C").to("D")
.edge("D").to("E")
.edge("E").to("A")
          .end()
    println(graph)
  }
}
