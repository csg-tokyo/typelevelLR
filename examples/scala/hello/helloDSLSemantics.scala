
object helloDSLSemantics {
    def runHelloDSL(parseTree: helloDSL.Start) = {
        parseTree match {
            case helloDSL.SimpleHello() => {
                println("Hello.")
            }
            case helloDSL.HelloWithName(name) => {
                println(s"Hello, $name!!")
            }
        }
    }
}
