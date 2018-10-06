# Minimal CMake configuration file to work with current emacs setup

# Compile with clang for company-clang & flycheck to work correctly
set (CMAKE_C_COMPILER             "/usr/bin/clang")
set (CMAKE_CXX_COMPILER             "/usr/bin/clang++")

cmake_minimum_required(VERSION 3.0)

project(cpp_intellisense_sandbox)

# Emit a compile_commands.json compilation db for irony to work with.
set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
set(CMAKE_BUILD_TYPE Debug)

# C++11
add_compile_options(-std=c++11)

add_library(my_base include/my_base.h src/my_base.cpp)
target_include_directories(my_base PUBLIC include)

add_executable(a main.cpp)
target_include_directories(a PRIVATE include)
target_link_libraries(a my_base)
