cmake_minimum_required(VERSION 3.10)

project(Fortran_Unittest LANGUAGES Fortran)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -Warray-temporaries -Wconversion -fimplicit-none -fbacktrace -fcheck=all -ffpe-trap=zero,overflow,underflow -finit-real=nan")
  else ()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffpe-summary=none -Ofast")
  endif ()
endif ()

set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")
include_directories(${CMAKE_BINARY_DIR})

set(sources
  src/assert_couter_mod.F90
  src/assert_result_mod.F90
  src/case_assert_couter_mod.F90
  src/case_couter_mod.F90
  src/couter_type_mod.F90
  src/unittest_mod.F90
  lib/shr_kind_mod.F90
  lib/string_type_mod.F90
  test/test_unittest.F90
)

add_library(unittest SHARED ${sources})
add_executable(test_unittest.exe ${sources})