
#pragma once

#include <iostream>
#include <memory>
#include "helloDSL.hpp"

namespace helloDSLSemantics {

///////////////////////////////////////////////////////////////////////////////

class HelloDSLSemantics : public helloDSL::Start::ConstVisitor {
public:
    void visitSimpleHello(const helloDSL::SimpleHello& host) {
        std::cout << "Hello." << std::endl;
    }
    void visitHelloWithName(const helloDSL::HelloWithName& host) {
        std::cout << "Hello, " << std::get<0>(host) << "!!" << std::endl;
    }
};

void runHelloDSL(const std::shared_ptr<helloDSL::Start>& parseTree) {
    HelloDSLSemantics visitor;
    parseTree->accept(visitor);
}

///////////////////////////////////////////////////////////////////////////////

}
