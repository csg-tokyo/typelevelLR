import * as helloDSL from "./_helloDSL"
helloDSL.begin()
    .hello()
    .name("OK")
    .end()
    .accept(new class visitor extends helloDSL.Visitor {
        visitHelloWithName(h: helloDSL.HelloWithName) {
            console.log(h.name)
        }
    }())

helloDSL.begin().hello().name("ok").end().accept()

helloDSL.begin()
    .hello()
    .end()
    .accept(new class visitor extends helloDSL.Visitor {
        visitSimpleHello() {
            console.log("Simple")
        }
    }())
