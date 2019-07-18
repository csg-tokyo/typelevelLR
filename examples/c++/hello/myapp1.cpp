
#include <iostream>
#include <memory>
#include "helloDSL.hpp"
using namespace helloDSL;

int main() {
    std::shared_ptr<Start> parseTree = begin()->hello()->name("ymzk")->end();
    std::cout << *parseTree << std::endl;
    return 0;
}
