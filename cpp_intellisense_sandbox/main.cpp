#include "my_base.h"
#include "derived_1.h"

#include <stdio.h>
#include <string>
#include <iostream>
#include <vector>

struct TH1F {
  TH1F(int a) {}
  void foo() {}
};

int main() {
  std::string a("ammar");
  std::cout << a << std::endl;
  MyBase mb;
  mb.MustDefine();
  mb.MustDefine_arg(2);
  Derived_1 dr;
  MyBase *d_1 = new Derived_1;
  d_1->MustDefine();
  std::string str;
  TH1F *h1 = new TH1F(5);
  h1->foo();
}
