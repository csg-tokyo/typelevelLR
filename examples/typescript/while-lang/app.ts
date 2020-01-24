import * as While from "./WhileLanguage"

While.begin()
    .var("ok")
    .assign()
    .num(100)
    .end()
    .accept()
