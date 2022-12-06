set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

set(
  srcs
  "${dir}/assert_counter_mod.F90"
  "${dir}/assert_result_mod.F90"
  "${dir}/case_assert_counter_mod.F90"
  "${dir}/case_counter_mod.F90"
  "${dir}/counter_type_mod.F90"
  "${dir}/unittest_type_mod.F90"
  "${dir}/unittest.F90"
)

set(
  MODULE_OUTPUT_DIR
  "${CMAKE_CURRENT_BINARY_DIR}/include"
)

# Create example library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)

set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${MODULE_OUTPUT_DIR}"
)
target_link_libraries(
  "${PROJECT_NAME}-lib"
  PRIVATE
  fortran_string
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  INTERFACE
  $<BUILD_INTERFACE:${MODULE_OUTPUT_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

if(INSTALL_PROJECT)
  # Export targets for other projects
  install(
    TARGETS
    "${PROJECT_NAME}-lib"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
  install(
    FILES
    "${MODULE_OUTPUT_DIR}/unittest.mod"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  )
endif()
