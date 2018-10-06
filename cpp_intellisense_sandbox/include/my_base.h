#pragma once
#include <vector>

class MyBase {
 public:
  MyBase();
  void MustDefine();
  // MustDefine_arg has some docs here.
  double MustDefine_arg(int a);

 private:
  std::vector<char> letters_;
};
