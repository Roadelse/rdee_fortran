
add_executable(test.node test.node.F90)
target_link_libraries(test.node
    rdee_fortran
)

add_executable(test.list test.list.F90)
target_link_libraries(test.list
    rdee_fortran
)

add_executable(test.dict test.dict.F90)
target_link_libraries(test.dict
    rdee_fortran
)



add_test(
    NAME test.node
    COMMAND $<TARGET_FILE:test.node>
)
add_test(
    NAME test.list
    COMMAND $<TARGET_FILE:test.list>
)
add_test(
    NAME test.dict
    COMMAND $<TARGET_FILE:test.dict>
)