
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->end() << std::endl;
  return 0;
}
