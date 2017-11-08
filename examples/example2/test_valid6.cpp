
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->num( 1 )->mul()->lp()->num( 2 )->add()->num( 3 )->rp()->mul()->num( 4 )->end() << std::endl;
  return 0;
}
