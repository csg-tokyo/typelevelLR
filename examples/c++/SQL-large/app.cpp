#include "SQL.hpp"
#include <iostream>

using namespace SQL;

int main() {
    auto parseTree = begin()->select()->;
    std::cout << *parseTree
        << std::endl;
}
