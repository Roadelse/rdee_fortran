
Program test_time
    use rdee_fortran
    implicit none

    call test_now_str()

contains

    subroutine test_now_str()
        implicit none

        print *, now_str()

    end subroutine

End Program