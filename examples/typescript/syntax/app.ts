import * as Syntax from "./Syntax"

const syntax = Syntax.begin()
    .syntax("Syntax").startsWith("Start")
    .rule("DefineSyntax").derive("Syntax").to("To")
        .andThen("then").andThen("then")
    .rule("RulesCons").derive("derive").to("To").andThen("then")
    .rule("RulesNull").derive("derive").toEpsilon()
    .rule("RuleDerive").derive("derive").to("To").andThen("then")
    .rule("RuleTo").derive("derive").to("To").andThen("then")
    .rule("RuleToEpsilon").derive("derive").to("To")
    .rule("RuleTailTo").derive("derive").to("To").andThen("then")
    .rule("RuleTailEpsilon").derive("derive").toEpsilon()
    .end()

syntax.accept()
