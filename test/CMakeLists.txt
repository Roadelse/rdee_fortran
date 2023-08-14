
# link_libraries(rdee_fortran)
# get_filename_component(BFC ${CMAKE_Fortran_COMPILER} NAME)

if ((DEFINED ${inSW} AND ${inSW}) OR (DEFINED $ENV{inSW} AND $ENV{inSW}))
    set(inSW TRUE)   
    message(STATUS "\033[31m Sunway detected! \033[0m")
else()
    set(inSW FALSE)
endif()

function(ecsl_test testID)  # encapsulation of test
    add_executable(test.${testID} test.${testID}.F90)
    target_link_libraries(test.${testID} rdee_fortran)
    target_compile_options(test.${testID} PUBLIC -g)
    
    if (${inSW})
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

ecsl_test(node)
ecsl_test(list)
ecsl_test(dict)
ecsl_test(array)
ecsl_test(string)
ecsl_test(time)

