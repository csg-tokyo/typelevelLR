
#include <iostream>
#include "example3.hpp"
using namespace example3;

int main() {
  // 14 = 0b1110
  std::cout << *begin()->I()->I()->I()->O()->end() << std::endl;
  return 0;
}
