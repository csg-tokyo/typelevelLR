import * as helloDSL from "./helloDSL"
helloDSL.begin()
    .hello()
    .name("OK")
    .end()
    .accept(new class visitor extends helloDSL.Visitor {
        visitHelloWithName(h: helloDSL.HelloWithName) {
            console.log(h.name)
        }
        visitSimpleHello() {
            console.log("Simple")
        }
    }())
