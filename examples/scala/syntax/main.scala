
object main {
  import Syntax._
  def main(args: Array[String]) = {
    val parseTree = begin().syntax("a").startsWith("b").rule("c").derive("d").toEpsilon().rule("e").derive("f").to("g").andThen("h").andThen("i").andThen("j").andThen("k").andThen("l").rule("m").derive("n").to("o").andThen("p").rule("q").derive("r").to("s").end()
    println(parseTree)
  }
}
