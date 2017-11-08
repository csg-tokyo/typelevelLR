
#include <iostream>
#include "example3.hpp"
using namespace example3;

int main() {
  // 1785 = 0b11011111001
  std::cout << *begin()->I()->I()->O()->I()->I()->I()->I()->I()->O()->O()->I()->end() << std::endl;
  return 0;
}
