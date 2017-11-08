
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->num( 1 )->mul()->num( 2 )->add()->num( 3 )->mul()->num( 4 )->end() << std::endl;
  return 0;
}
