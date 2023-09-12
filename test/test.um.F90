Program main
    use rdee_base
    implicit none

    integer :: nargs

    nargs = command_argument_count()

    if (nargs .eq. 0) then
        call test_um_ammd
    else
        
    end if



contains




end Program