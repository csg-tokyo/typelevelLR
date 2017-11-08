
#include <iostream>
#include "example3.hpp"
using namespace example3;

int main() {
  // 35 = 0b100011
  std::cout << *begin()->I()->O()->O()->O()->I()->I()->end() << std::endl;
  return 0;
}
