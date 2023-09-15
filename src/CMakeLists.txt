



execute_process(COMMAND ${Python3_EXECUTABLE} ${SRC}/jj2/render-jj2.py)


file(GLOB SRCS *.F90 *.f90 *.F *.f)
# set(SRCS rdee_base.F90 rdee_um.F90 rdee_algo.F90 rdee_ds.F90 rdee_time.F90 rdee_string.F90)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    # set_property(SOURCE rdee_um.F90 APPEND_STRING PROPERTY COMPILE_OPTIONS -Wno-conversion)
    set_source_files_properties(rdee_um.F90 PROPERTIES COMPILE_OPTIONS -Wno-conversion)
endif()

message(STATUS "§ ${Cyan}(/src/)${ColorReset} source files = ${SRCS}")
add_library(rdee_fortran STATIC ${SRCS})




