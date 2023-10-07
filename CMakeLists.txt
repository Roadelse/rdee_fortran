
cmake_minimum_required(VERSION 3.10)

project(rdee_fortran VERSION 0.1 LANGUAGES Fortran)


#set(CMAKE_Fortran_COMPILER ifort)
message("compiler=${CMAKE_Fortran_COMPILER}")


#find_package(Python3 COMPONENTS Interpreter) # Development)
#find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
list(PREPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)
include(rdee.colorful)
include(rdee.python)

check_python_lib(jinja2)
check_python_lib(numpy)

# set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_ROOT}/lib)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)
#set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

SET(SRC ${CMAKE_SOURCE_DIR}/src)

enable_testing()

Message(STATUS "¦ compiler id = ${CMAKE_Fortran_COMPILER_ID}")
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    add_compile_options(-g -fPIE "SHELL:-check all" "SHELL:-warn all" -std08 "SHELL:-warn nounused")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    add_compile_options(-g -fPIE -ffree-line-length-none -std=f2008 -Wall -fcheck=all -fall-intrinsics -Wno-unused -Wno-maybe-uninitialized)
endif()

get_filename_component(BFC ${CMAKE_Fortran_COMPILER} NAME)
string(SUBSTRING ${BFC} 0 3 compiler_prefix)
if (${compiler_prefix} STREQUAL "mpi")
    add_definitions(-DMPI)
    message(STATUS "§ ${Cyan}(MPI)${ColorReset} add -DMPI definition")
endif()


add_subdirectory(src)
add_subdirectory(test)

