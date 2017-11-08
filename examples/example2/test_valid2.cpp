
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->num( 1 )->add()->num( 2 )->mul()->num( 3 )->add()->num( 4 )->end() << std::endl;
  return 0;
}
