Program main
    use rdee_fortran
    implicit none

    call test_argwhere_c1s
    call test_toString

contains
    subroutine test_argwhere_c1s()
        character(80) :: s1
        integer, allocatable :: iaa1(:)

        s1 = 'hello, im fine, thank you and you'


        call argwhere_c1s(s1, 'o', iaa1)

        print *, iaa1
        call assert(all(iaa1 .eq. [5, 24, 32]), 'Error in string/argwhere_c1s')
    end subroutine

    subroutine test_toString()
        implicit none
        character(80) :: s1

        s1 = 'now'

        print *, toString('what', 1, 3.14, 'now', cmplx(1,2), 'wow', nSpace=1)
        call assert(toString('now','you') .eq. 'nowyou', 'Error in rdee_string/toString, test 1')
        call assert(toString(s1, 1, 'you', nSpace=1) .eq. 'now 1 you', 'Error in rdee_string/toString, test 2')
    end subroutine

End Program
