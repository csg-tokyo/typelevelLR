#include <iostream>
#include <memory>
#include "DotLanguage.hpp"
using namespace DotLanguage;

int main() {
    std::shared_ptr<Graph> parseTree = begin()
        ->digraph("small_graph")->node("A")->shape("rectangle")
        ->node("B")
        ->node("C")
        ->shape("doublecircle")
        ->edge("A")
        ->to("B")->end();
    std::cout << *parseTree << std::endl;
    return 0;
}