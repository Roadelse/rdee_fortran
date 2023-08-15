
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

add_subdirectory(src)
add_subdirectory(test)

