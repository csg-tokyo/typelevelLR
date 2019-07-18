
object myApp1 {
    import helloDSL._

    def main(args: Array[String]) = {
        val parseTree: Start = begin().hello().name("ymzk").end()
        println(parseTree)
    }
}
