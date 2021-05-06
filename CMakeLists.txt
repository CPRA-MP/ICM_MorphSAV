###########################################################################
#                 CMake Build File for ICM-Morph
#
#    Written By: Zach Cobell & Eric White
#
###########################################################################
#
# The CMake build system enable ICM-Morph to be deployed and built
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
PROJECT(ICM_Morph Fortran)
ENABLE_LANGUAGE( C )

###########################################################################


###########################################################################
#...Put the static libraries in the CMakeFiles folder so they don't
#   contaminate the build directory
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles)
###########################################################################


###########################################################################
#...Set the version string (used for libraries)
SET(ICMMORPH_VERSION_MAJOR 23)
SET(ICMMORPH_VERSION_MINOR  1)
SET(ICMMORPH_VERSION_PATCH  0)
SET(ICMMORPH_VERSION_STRING ${ICMMORPH_VERSION_MAJOR}.${ICMMORPH_VERSION_MINOR}.${ICMMORPH_VERSION_PATCH})
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
# ICM_Morph
###########################################################################
SET( MORPH morph_v${ICMMORPH_VERSION_STRING} )
ADD_EXECUTABLE( ${MORPH} WM_distance_to_land.f90
                         WM_edge_delineation.f90
                         WM_edge_erosion.f90
                         WM_flotant.f90
                         WM_inundation_depths.f90
                         WM_inundation_HSI_bins.f90
                         WM_inundation_thresholds.f90
                         WM_main.f90
                         WM_map_bareground.f90
                         WM_mineral_deposition.f90
                         WM_new_subaerial_land.f90
                         WM_organic_accretion.f90
                         WM_params.f90
                         WM_params_alloc.f90
                         WM_preprocessing.f90
                         WM_sav.f90
                         WM_set_io.f90
                         WM_summaries.f90
                         WM_update_elevation.f90
                         WM_update_landtype.f90
                         WM_write_output_asci_rasters.f90
                         WM_write_output_binary_rasters.f90
                         WM_write_output_QAQC_points.f90
                         WM_write_output_summaries.f90      )
SET_TARGET_PROPERTIES( ${MORPH} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/mod)
SET_TARGET_PROPERTIES( ${MORPH} PROPERTIES COMPILE_FLAGS "${Fortran_LINELENGTH_FLAG} ${Fortran_COMPILER_SPECIFIC_FLAG}" )

IF(ENABLE_OUTPUT_NETCDF)
    TARGET_COMPILE_DEFINITIONS( ${MORPH} PRIVATE ICM_NETCDF )
    TARGET_INCLUDE_DIRECTORIES( ${MORPH} PRIVATE ${NETCDF_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES( ${MORPH} PRIVATE ${NETCDF_LIBRARIES})
ENDIF()