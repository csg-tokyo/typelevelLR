
#include <iostream>
#include "example3.hpp"
using namespace example3;

int main() {
  // 105 = 0b1101001
  std::cout << *begin()->I()->I()->O()->I()->O()->O()->I()->end() << std::endl;
  return 0;
}
