#include <my_base.h>
#include "derived_1.h"

#include <stdio.h>
#include <string>
#include <iostream>

int main() {
  std::string a("ammar");
  std::cout << a << std::endl;
  MyBase mb;
  mb.MustDefine();
  mb.MustDefine_arg(2);
  Derived_1 dr;
  MyBase* d_1 = new Derived_1;
  d_1->MustDefine();
}
