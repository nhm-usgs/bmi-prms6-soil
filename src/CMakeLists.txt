add_library(${bmiprms6soil_lib} SHARED
  bmi.f90 bmi_prms_soil.f90 m_prms_soil.f90)
target_link_libraries(${bmiprms6soil_lib} ${EXTRA_LIBS})

install(
  TARGETS ${bmiprms6soil_lib}
  EXPORT ${bmiprms6soil_lib}-config
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  INCLUDES DESTINATION include)

install(
  DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
  PATTERN "CMakeFiles" EXCLUDE)

install(
  EXPORT ${bmiprms6soil_lib}-config
  DESTINATION lib/cmake)
export(
  TARGETS ${bmiprms6soil_lib}
  FILE ${bmiprms6soil_lib}-config.cmake)
