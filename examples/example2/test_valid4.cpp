
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->lp()->lp()->lp()->num( 98 )->rp()->rp()->rp()->end() << std::endl;
  return 0;
}
