
#include "exam1.hpp"
#include <iostream>
using namespace std;
using namespace exam1;

int main() {
  auto graph = begin()
    ->digraph("a small graph")
      ->node("A")->shape("rectangle")
      ->node("B")
      ->node("C")->shape("doublecircle")
      ->edge("A")->to("B")->style("dotted")
      ->edge("A")->and_("B")->to("C")
    ->end();
  cout << *graph << endl;
  return 0;
}
