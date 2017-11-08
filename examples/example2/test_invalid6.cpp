
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->lp()->lp()->num( 42 )->rp()->end() << std::endl;
  return 0;
}
