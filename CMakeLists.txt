cmake_minimum_required(VERSION 3.12)

project("fortran_unittest" LANGUAGES "Fortran" VERSION "0.0.1")

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)
option(BUILD_SHARED_LIBS "Whether shared libraries should be built" TRUE)
option(BUILD_TESTING "Whether the test executable should be compiled" FALSE)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

include(cmake/compilers.cmake)

# Add third-party library
add_subdirectory("lib/Fortran_String")

# Collect source of the project
add_subdirectory("src")

if(INSTALL_PROJECT)
  # Export targets for other projects
  add_library("${PROJECT_NAME}" INTERFACE)
  target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
  install(
    TARGETS
    "${PROJECT_NAME}"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )

  include(cmake/install.cmake)
endif()

if(BUILD_TESTING)
  add_executable(test_unittest.exe)
  target_sources(test_unittest.exe PRIVATE "test/test_unittest.F90")
  target_link_libraries(test_unittest.exe PRIVATE ${PROJECT_NAME} fortran_string)
endif()