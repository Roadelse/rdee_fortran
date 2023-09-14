Program main
    use rdee_base
    use rdee_um
    implicit none

    integer :: nargs

    nargs = command_argument_count()

    if (nargs .eq. 0) then
        call test_um_ammd
    else
        continue
    end if



contains

    subroutine test_um_ammd()
        implicit none
        class(*), allocatable :: uma, umb, umc

        uma = 2
        umb = 1.4d0

        umc = fum_add(uma, umb)
        call fum_print(umc)
        call assert(um2r8(umc) .eq. 2 + 1.4d0, 'Error in test_um_ammd')

    end subroutine




end Program