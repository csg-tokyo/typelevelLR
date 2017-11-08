
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->lp()->num( 1 )->add()->num( 2 )->rp()->mul()->lp()->num( 3 )->add()->num( 4 )->rp()->end() << std::endl;
  return 0;
}
