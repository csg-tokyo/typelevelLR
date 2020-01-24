import * as helloDSL from "./helloDSL"
helloDSL.begin()
    .hello()
    .name("OK")
    .end()
    .accept(new class visitor extends helloDSL.DefaultVisitor {
        visitHelloWithName(h : helloDSL.HelloWithName) {
            process.stdout.write("hello ")
            h.arg1.accept(this)
        }
        visitNameString(h : helloDSL.NameString) {
            console.log(h.arg1, "!")
        }
    }())

helloDSL.begin().hello().name("ok").end().accept()

helloDSL.begin()
    .hello()
    .end()
    .accept(new class visitor extends helloDSL.DefaultVisitor {
        visitSimpleHello() {
            console.log("Simple Hello!")
        }
    }())
