Program main
    use rdee_fortran
    implicit none


    call test_ispan
    call test_fspan


Contains
    Subroutine test_ispan()
        implicit none
        
        call assert(all(ispan(1,3) .eq. [1,2,3]), 'Error in ispan')
        call assert(all(ispan(1,3,2) .eq. [1,3]), 'Error in ispan')
        call assert(all(ispan(1,4,2) .eq. [1,3]), 'Error in ispan')
        print *, 'succeed in test for ispan'

    End Subroutine

    Subroutine test_fspan()
        implicit none
       
        
        call assert(all(fspan(1.5,3.5, 5) .eq. [1.5,2., 2.5,3.,3.5]), 'Error in fspan')
        print *, 'succeed in test for fspan'

    End Subroutine
End Program

