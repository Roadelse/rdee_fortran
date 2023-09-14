



execute_process(COMMAND ${Python3_EXECUTABLE} ${SRC}/jj2/render-jj2.py)


file(GLOB SRCS *.F90 *.f90 *.F *.f)
# set(SRCS rdee_base.F90 rdee_um.F90 rdee_algo.F90 rdee_ds.F90 rdee_time.F90 rdee_string.F90)
message(STATUS "§ ${Cyan}(/src/)${ColorReset} source files = ${SRCS}")
add_library(rdee_fortran STATIC ${SRCS})




