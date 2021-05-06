###########################################################################
#                 CMake Build File for ICM-Morph
#
#    Written By: Zach Cobell & Eric White
#
###########################################################################
#
# The CMake build system enable ICM-MorphSAV to be deployed and built
# in a cross platform environment. 
#
###########################################################################
#...Set the default build type
IF(DEFINED CMAKE_BUILD_TYPE)
    SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of
        build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug
        Release RelWithDebInfo MinSizeRel.")
ELSE()
    SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build,
        options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release
        RelWithDebInfo MinSizeRel.")
ENDIF()
###########################################################################

###########################################################################
#  GENERAL OPTIONS
###########################################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12)
PROJECT(ICM_MorphSAV Fortran)
ENABLE_LANGUAGE( C )

###########################################################################


###########################################################################
#...Put the static libraries in the CMakeFiles folder so they don't
#   contaminate the build directory
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
###########################################################################


###########################################################################
#...Set the version string (used for libraries)
SET(ICMMORPHSAV_VERSION_MAJOR 23)
SET(ICMMORPHSAV_VERSION_MINOR  1)
SET(ICMMORPHSAV_VERSION_PATCH  0)
SET(ICMMORPHSAV_VERSION_STRING ${ICMMORPHSAV_VERSION_MAJOR}.${ICMMORPHSAV_VERSION_MINOR}.${ICMMORPHSAV_VERSION_PATCH})
###########################################################################


###########################################################################
#...Determine architecture specific parameters
INCLUDE(${CMAKE_SOURCE_DIR}/cmake/architecture.cmake)
###########################################################################


###########################################################################
#...Enable/Disable netCDF functionality
OPTION(ENABLE_OUTPUT_NETCDF "Enable netCDF output formats" OFF)
IF(ENABLE_OUTPUT_NETCDF)
    SET(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
    SET(NETCDF_F90 "YES")
    FIND_PACKAGE(NetCDF)
    IF(NOT NETCDF_FOUND)
        MESSAGE(SEND_ERROR "Could not locate NetCDF. Try setting the command line flag -DNETCDF_DIR=[path]")
    ENDIF()
ENDIF()
###########################################################################


###########################################################################
# ICM_MorphSAV
###########################################################################
SET( MORPHSAV morphSAV_v${ICMMORPHSAV_VERSION_STRING} )
ADD_EXECUTABLE( ${MORPHSAV} WMsav_distance_to_land.f90
                         WMsav_main.f90
                         WMsav_params.f90
                         WMsav_params_alloc.f90
                         WMsav_preprocessing.f90
                         WMsav_sav.f90
                         WMsav_set_io.f90 )
SET_TARGET_PROPERTIES( ${MORPHSAV} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/mod)
SET_TARGET_PROPERTIES( ${MORPHSAV} PROPERTIES COMPILE_FLAGS "${Fortran_LINELENGTH_FLAG} ${Fortran_COMPILER_SPECIFIC_FLAG}" )

IF(ENABLE_OUTPUT_NETCDF)
    TARGET_COMPILE_DEFINITIONS( ${MORPHSAV} PRIVATE ICM_NETCDF )
    TARGET_INCLUDE_DIRECTORIES( ${MORPHSAV} PRIVATE ${NETCDF_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES( ${MORPHSAV} PRIVATE ${NETCDF_LIBRARIES})
ENDIF()