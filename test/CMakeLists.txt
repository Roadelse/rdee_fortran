
include(rdee.checksys)

set_source_files_properties(test.array.F90 PROPERTIES COMPILE_OPTIONS -Wno-maybe-uninitialized)

function(ecsl_test testID)  # encapsulation of test
    add_executable(test.${testID} test.${testID}.F90)
    target_link_libraries(test.${testID} rdee_fortran)
    target_compile_options(test.${testID} PUBLIC -g)
    
    if (inSW)
        add_test(
            NAME test.${testID}
            COMMAND bash -c "bsub -b -I -q q_sw_expr -n 1 -cgsp 64 -share_size 7000 -host_stack 512 $<TARGET_FILE:test.${testID}>"
        )
    else()
        add_test(
            NAME test.${testID}
            COMMAND $<TARGET_FILE:test.${testID}>
        )
    endif()

endfunction()

ecsl_test(um)
ecsl_test(node)
ecsl_test(list)
ecsl_test(dict)
ecsl_test(array)
ecsl_test(string)
ecsl_test(time)

