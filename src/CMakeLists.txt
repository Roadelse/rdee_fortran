

#set(SRCS rdee_base.F90 rdee_algo.F90 rdee_ds.F90 rdee.F90)

Message(STATUS "compiler id = ${CMAKE_Fortran_COMPILER_ID}")
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    add_compile_options(-g -fPIE -ffree-line-length-none "SHELL:-check all" "SHELL:-warn all" -std08 "SHELL:-warn nounused")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    add_compile_options(-g -fPIE -std=f2008 -Wall -fcheck=all -fall-intrinsics -Wno-unused)
endif()


message("dir is ${CMAKE_BINARY_DIR}")
execute_process(COMMAND python3 ${SRC}/jj2/render-jj2.py)
#add_custom_command(OUTPUT rdee_ds.F90
#    COMMAND jj2_lsp=!#jj2 ${SRC}/render-jj2.py ${SRC}/rdee_ds.F90.jj2
#)
#add_custom_target(ds
#    DEPENDS rdee_ds.F90
#)


file(GLOB SRCS *.F90 *.f90 *.F *.f)
message(STATUS "srcs = ${SRCS}")
add_library(rdee_fortran STATIC ${SRCS})




