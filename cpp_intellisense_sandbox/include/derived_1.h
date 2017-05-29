#pragma once

#include "my_base.h"
#include <iostream>

class Derived_1 : public MyBase {
public:
  Derived_1() {
    std::cout << "Derived_1 MustDefine" << std::endl;
  }
};
