
#include <iostream>
#include "example2.hpp"
using namespace example2;

int main() {
  std::cout << *begin()->add()->end() << std::endl;
  return 0;
}
