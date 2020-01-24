#include <iostream>
#include <memory>
#include "helloDSL.hpp"
#include "helloDSLSemantics.hpp"
using namespace helloDSL;
using namespace helloDSLSemantics;

int main() {
    std::shared_ptr<Start> parseTree = begin()->hello()->name("Matts966")->end();
    runHelloDSL(parseTree);
    return 0;
}