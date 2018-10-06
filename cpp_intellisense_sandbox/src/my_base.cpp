#include "my_base.h"
#include <iostream>
#include <string>

using std::string;

MyBase::MyBase() {
  letters_.resize(4);
  letters_ = {'a', 'b', 'c', 'd'};
  std::cout << "Constructor" << std::endl;
  MustDefine();
}

void MyBase::MustDefine() { std::cout << "Implementation of MustDefine()" << std::endl; }

double MyBase::MustDefine_arg(int a) {
  string st("ammar");
  return a;
}
