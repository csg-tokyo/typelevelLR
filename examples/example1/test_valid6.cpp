
#include <iostream>
#include "example1.hpp"
using namespace example1;

int main() {
  std::cout << *begin()->a()->a()->b()->b()->end() << std::endl;
  return 0;
}
