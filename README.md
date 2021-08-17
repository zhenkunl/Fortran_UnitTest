<a name="top"></a>

# Fortran Unit Test Library
[![Open in Visual Studio Code](https://open.vscode.dev/badges/open-in-vscode.svg)](https://open.vscode.dev/github.com/zhenkunl/Fortran_UnitTest)

Unit test is a way of testing a program unit which can be isolated from the entire sofeware program. Most programming languages (like C++, Python) have had their own unit test framework already, though Fortran doesn't have such a library widely used by the community. `Fortran_UnitTest` is an attempt to implement a pure Fortran library using Object-Oriented Programming (OOP). It is strongly inspired by [Zofu](https://github.com/acroucher/zofu), and its output format is derived from [fortran-unit-test](https://github.com/dongli/fortran-unit-test). It, however, is totally a different library for its easy use which will be shown later. In this repository, all the operations are encapsulated in a Fortran class `unittest_t`, so it is very easily distinguished from functions or subroutines of the library to be tested. 

Hopefully Fortran_UnitTest will be able to provide a good alternative for unit test in Fortran.

## Getting started

### Get the code
```sh
git clone https://github.com/zhenkunl/Fortran_UnitTest
cd Fortran_UnitTest
git submodule init && git submodule update
```

### Requirements
To build the Fortran_UnitTest library you need
* Fortran 2008-compatible compiler
* CMake version 3.12 or newer

The build system has been tested on Mac and Linux using the Intel Fortran Compiler 2021.2.0 and gfortran 7.5.0.

### Build with CMake
Configure the build with
```sh
cmake -B build
```
You can pass additional options to CMake to customize the build.
Important options are
* `-DCMAKE_INSTALL_PREFIX` is used to provide the install location for the library.
* `-DBUILD_TESTING` set to `on` in case you want to generate a test executable against the library (by default: `off`).

For example, to configure a build using gfortran and create the test executable use
```sh
FC=gfortran cmake -B build -DCMAKE_INSTALL_PREFIX=your/library/path -DBUILD_TESTING=on
```
To build the library run
```sh
cmake --build build
```
To test your build, run the test executable under the `build` directory once the build has finished with
```sh
./build/test_unittest.exe
```
If everything goes well, to install the project to the declared prefix run
```sh
cmake --install build
```

Go to [Top](#top)

## Usage
To make use this library use the `unittest` module in your projects. Include a `use unittest` statement then a derived Fortran type `unittest_t` is available. A unit test object may then be declared. An example usage is shown below:
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
Also an complete [example program](https://github.com/zhenkunl/Fortran_UnitTest/blob/main/test/test_unittest.F90) is contained under the `test` subdirectory.

Go to [Top](#top)

## Output from Fortran_UnitTest
The output form is very like that of [fortran-unit-test](https://github.com/dongli/fortran-unit-test). The only difference is that the result of each single assertion is outputed immediately once it is completed in this repo, thus the summary of all assertions in one case is given at the end of the case. An example of output would look like this:
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
Go to [Top](#top)

## Using Fortran_UnitTest in your project
The Fortran_UnitTest project exports CMake package files to make it usable for other projects. The package files are located in the library directory in the installation prefix.

To build your own project which requires Fortran_UnitTest library, 
you can find a local installation in your `CMakeLists.txt` with
```cmake
find_package(fortran_unittest REQUIRED)
...
target_link_libraries(
  ${PROJECT_NAME}
  PRIVATE
  fortran_unittest::fortran_unittest
)
```
To make the installed library discoverable add the Fortran_UnitTest directory to the `CMAKE_PREFIX_PATH`. For Bash:
```sh
export CMAKE_PREFIX_PATH=your/library/path:$CMAKE_PREFIX_PATH
```
For csh:
```sh
setenv CMAKE_PREFIX_PATH your/library/path:$CMAKE_PREFIX_PATH
```
The usual install location of the package files is `$PREFIX/lib/cmake/fortran_unittest`.

You could, alternatively, specify the install location when configure your project by:
```sh
fortran_unittest_DIR=your/library/path cmake -B build [other options]
```
Go to [Top](#top)
