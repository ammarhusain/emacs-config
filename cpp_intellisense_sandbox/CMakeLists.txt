cmake_minimum_required(VERSION 3.0)

project(cpp_intellisense_sandbox)

add_library(my_base include/my_base.h src/my_base.cpp)
target_include_directories(my_base PUBLIC include)

add_executable(a main.cpp)
target_include_directories(a PRIVATE include)
target_link_libraries(a my_base)
