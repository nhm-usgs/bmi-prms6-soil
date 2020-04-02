cmake_minimum_required(VERSION 2.8)

project(bmi-prms6-soil Fortran)

set(bmiprms6soil_lib bmiprmssoil)

# Set compiler flags for gfortran.
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -fall-intrinsics")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -fcheck=all -Wall")
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -funroll-all-loops -finline-functions")
endif()

# Locate the installed diffusionf library (via CMAKE_PREFIX_PATH).
find_library(prms6_lib prmslib)
find_library(coretran_lib coretran)
find_library(netcdf_lib netcdf)
find_library(netcdff_lib netcdff)
set(EXTRA_LIBS
  ${prms6_lib}
  ${coretran_lib}
  ${netcdf_lib}
  ${netcdff_lib})

# Locate the installed diffusion module (via CMAKE_PREFIX_PATH).
find_path(prmslibmod utils_prms.mod)
find_path(coretranmod variablekind.mod HINTS ${prmslibmod}/coretran)
include_directories(${prmslibmod} ${coretranmod})

message(STATUS "prms: ${prms6_lib}")
message(STATUS "coretran: ${coretran_lib}")
message(STATUS "netcdf: ${netcdf_lib}")
message(STATUS "netcdff: ${netcdff_lib}")
message(STATUS "prms include: ${prmslibmod}")
message(STATUS "coretran include: ${coretranmod}")

add_subdirectory(src)
add_subdirectory(tests)

include(CTest)
