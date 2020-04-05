cmake_minimum_required(VERSION 2.8)

project(bmi-prms6-soil Fortran)

set(bmiprms6soil_lib bmiprmssoil)

# Locate the installed diffusionf library (via CMAKE_PREFIX_PATH).
find_library(prms6_lib prmslib)
find_library(bmiprms6surface_lib bmiprmssurface)
find_library(coretran_lib coretran)
find_library(netcdf_lib netcdf)
find_library(netcdff_lib netcdff)
set(EXTRA_LIBS
  ${prms6_lib}
  ${bmiprsm6surface_lib}
  ${coretran_lib}
  ${netcdf_lib}
  ${netcdff_lib})

# Locate the installed diffusion module (via CMAKE_PREFIX_PATH).
find_path(prmslibmod utils_prms.mod)
find_path(prmssurfacelibmod bmiprmssurface.mod)
find_path(coretranmod variablekind.mod HINTS ${prmslibmod}/coretran)
include_directories(${prmslibmod} ${prmssurfacelibmod} ${coretranmod})

message(STATUS "prms: ${prms6_lib}")
message(STATUS "prmssurface: ${bmiprsm6surface_lib}")
message(STATUS "coretran: ${coretran_lib}")
message(STATUS "netcdf: ${netcdf_lib}")
message(STATUS "netcdff: ${netcdff_lib}")
message(STATUS "prms include: ${prmslibmod}")
message(STATUS "prmssurface include: ${prmssurfacelibmod}")
message(STATUS "coretran include: ${coretranmod}")

add_subdirectory(src)
add_subdirectory(tests)
add_subdirectory(examples)

include(CTest)
