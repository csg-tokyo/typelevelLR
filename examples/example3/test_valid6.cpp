
#include <iostream>
#include "example3.hpp"
using namespace example3;

int main() {
  // 7 = 0b7
  std::cout << *begin()->O()->O()->O()->O()->O()->I()->I()->I()->end() << std::endl;
  return 0;
}
