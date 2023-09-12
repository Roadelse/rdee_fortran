
Message(STATUS "¦ compiler id = ${CMAKE_Fortran_COMPILER_ID}")
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    add_compile_options(-O3 -fPIE "SHELL:-check all" "SHELL:-warn all" -std08 "SHELL:-warn nounused")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    add_compile_options(-O3 -fPIE -ffree-line-length-none -std=f2008 -Wall -fcheck=all -fall-intrinsics -Wno-unused)
endif()


execute_process(COMMAND ${Python3_EXECUTABLE} ${SRC}/jj2/render-jj2.py)


file(GLOB SRCS *.F90 *.f90 *.F *.f)
message(STATUS "§ ${Cyan}(/src/)${ColorReset} source files = ${SRCS}")
add_library(rdee_fortran STATIC ${SRCS})




