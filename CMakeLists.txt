cmake_minimum_required(VERSION 3.21)

project("fortran_unittest" LANGUAGES "Fortran" VERSION "0.0.1")

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)
option(BUILD_TESTING "Whether the test executable should be compiled" FALSE)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# Add flag for different compilers
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -gdwarf -Og -fbacktrace -fcheck=all")
    #-g -O0 -cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
    #-O3 -cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES PGI)
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -fPIC")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -check all")
    #-g -O0 -debug  -CB -CA -CU -std08 -fpp -fp-model source
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
    #-O3 -std08 -unroll -fpp -fp-model source -diag-disable 8291,8577
  endif()
endif()

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

  # Install exported targets
  install(
    EXPORT "${PROJECT_NAME}-targets"
    NAMESPACE
    "${PROJECT_NAME}::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

  include(CMakePackageConfigHelpers)
# generate the config file that is includes the exports
  configure_file(Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
  )

# generate the version file for the config file
  write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    VERSION ${VERSION}
    COMPATIBILITY AnyNewerVersion
  )

# install the configuration file
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
    DESTINATION lib/cmake/${PROJECT_NAME}
    )

endif()

if(BUILD_TESTING)
  add_executable(test_unittest.exe)
  target_sources(test_unittest.exe PRIVATE "test/test_unittest.F90")
  target_link_libraries(test_unittest.exe PRIVATE ${PROJECT_NAME} fortran_string)
endif()