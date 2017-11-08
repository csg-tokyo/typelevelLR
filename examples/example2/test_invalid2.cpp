
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->num( 1 )->num( 2 )->num( 3 )->end() << std::endl;
  return 0;
}
