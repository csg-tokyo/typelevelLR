
import scala.language.implicitConversions

object myApp2 {
    import helloDSL._
    import helloDSLSemantics._

    def main(args: Array[String]) = {
        val parseTree: Start = begin().hello().name("ymzk").end()
        runHelloDSL(parseTree)
    }
}
