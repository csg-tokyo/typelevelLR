
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->num( 1 )->add()->end() << std::endl;
  return 0;
}
