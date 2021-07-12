# Fortran Unit Test Library
Unit test is a way of testing a program unit which can be isolated from the entire sofeware program. Most program languages (like C++, Python) have had their own unit test framework already, though Fortran doesn't have such a library widely used by the community. This repository is an attempt to implement a pure Fortran library using Object-Oriented Programming (OOP). It is strongly inspired by [Zofu](https://github.com/acroucher/zofu), and its output format is derived from [fortran-unit-test](https://github.com/dongli/fortran-unit-test). However, it is totally a different library for its easy use which will be shown later. In this repository, all the operations are encapsulated in a Fortran class `unittest_t`, so it is very easily distinguished from functions or subroutines of the library to be tested.

## Basic usage
An [example program](https://github.com/zhenkunl/Fortran_UnitTest/blob/main/test/test_unittest.F90) showing its usage is included in the test subdirectory. Below is a code snippet explained its basic usage:
```Fortran
  use unittest, only : unittest_t
  type(unittest_t) :: test
  call test%init()
  call test%start_case()
  call test%assert_true(2>1, __FILE__, __LINE__)
  call test%assert_false(1<2, __FILE__, __LINE__)
  call test%end_case()
  call test%summary()
```

## Output format
The output format is very like that of [fortran-unit-test](https://github.com/dongli/fortran-unit-test). The only difference is that the result of each single assertion is outputed immediately once it is completed, thus the summary of all assertions in one case is given at the end of the case. An example of output is as follows.
```text
/////////////////////////// Report of Suite: suite1 /////////////////////////////

 +-> Details:
 |   |
 |   +-> case case1:
 |   |   |
 |   |   +-> Assertion #1 succeed!
 |   |   |
 |   |   +-> Assertion #2 failed with reason: x (True) false y (N/A)
 |   |   +-> Check line: /data_1/fortran-repos/Fortran_UnitTest/test/test_unittest.F90:10
 |   |   |
 |   +-> case1: 1 of 2 assertions succeed.
 |   |
 |   +-> case case2:
 |   |   |
 |   |   +-> Assertion #1 failed with reason: x (True) false y (N/A)
 |   |   +-> Check line: /data_1/fortran-repos/Fortran_UnitTest/test/test_unittest.F90:13
 |   |   |
 |   |   +-> Assertion #2 failed with reason: x (False) true y (N/A)
 |   |   +-> Check line: /data_1/fortran-repos/Fortran_UnitTest/test/test_unittest.F90:14
 |   |   |
 |   +-> case2: 0 of 2 assertions succeed.
 |   |
 |
 +-> Summary:
 |   +-> suite1:  1 of 4 assertions succeed.
 |   +-> suite1:  0 of 2 cases succeed.

////////////////////////////////////////////////////////////////////////////////
```