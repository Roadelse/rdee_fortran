Program main
    use rdee_fortran
    implicit none

    character(80) :: s1
    integer, allocatable :: iaa1(:)

    s1 = 'hello, im fine, thank you and you'


    call argwhere_c1s(s1, 'o', iaa1)

    print *, iaa1


End Program