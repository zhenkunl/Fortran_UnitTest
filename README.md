# Fortran Unit Test Library
[![Open in Visual Studio Code](https://open.vscode.dev/badges/open-in-vscode.svg)](https://open.vscode.dev/github.com/zhenkunl/Fortran_UnitTest)

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
  call test%assert_equal(1, 1, __FILE__, __LINE__)
  call test%assert_equal(1.1, 1.1, __FILE__, __LINE__)
  call test%assert_equal('abc', 'abcd', __FILE__, __LINE__)
  call test%assert_equal(.true., .true., __FILE__, __LINE__)
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
 |   |   +-> Check line: test_unittest.F90:10
 |   |   |
 |   |   +-> Assertion #3 succeed!
 |   |   |
 |   |   +-> Assertion #4 failed with reason: x (1) = y (2)
 |   |   +-> Check line: test_unittest.F90:12
 |   |   |
 |   |   +-> Assertion #5 succeed!
 |   |   |
 |   |   +-> Assertion #6 failed with reason: x (1.100000) = y (1.200000)
 |   |   +-> Check line: test_unittest.F90:14
 |   |   |
 |   |   +-> Assertion #7 succeed!
 |   |   |
 |   |   +-> Assertion #8 failed with reason: x (abc) = y (abcd)
 |   |   +-> Check line: test_unittest.F90:16
 |   |   |
 |   |   +-> Assertion #9 succeed!
 |   |   |
 |   |   +-> Assertion #10 failed with reason: x (True) = y (False)
 |   |   +-> Check line: test_unittest.F90:18
 |   |   |
 |   +-> case1: 5 of 10 assertions succeed.
 |   |
 |   +-> case case2:
 |   |   |
 |   |   +-> Assertion #1 failed with reason: x (True) false y (N/A)
 |   |   +-> Check line: test_unittest.F90:21
 |   |   |
 |   |   +-> Assertion #2 failed with reason: x (False) true y (N/A)
 |   |   +-> Check line: test_unittest.F90:22
 |   |   |
 |   +-> case2: 0 of 2 assertions succeed.
 |   |
 |
 +-> Summary:
 |   +-> suite1:  5 of 12 assertions succeed.
 |   +-> suite1:  0 of 2 cases succeed.

////////////////////////////////////////////////////////////////////////////////
```