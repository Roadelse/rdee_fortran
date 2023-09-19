Program main
    use rdee_fortran
    implicit none
    character(64) :: arg1


    call get_command_argument(1, arg1)

    if (arg1 .eq. 'ALL' .or. arg1 .eq. 'all' .or. arg1 .eq. '') then
        call test_argwhere_c1s
        call test_toString
        call test_string1dBuilder
    elseif (arg1 .eq. 'test_argwhere_c1s') then
        call test_argwhere_c1s
    elseif (arg1 .eq. 'test_toString') then
        call test_toString
    elseif (arg1 .eq. 'test_string1dBuilder') then
        call test_string1dBuilder
    else
        print *, 'unknwon argument!'
        stop 1
    end if 

contains
    subroutine test_argwhere_c1s()
        character(80) :: s1
        integer, allocatable :: iaa1(:)

        print *, ''
        print *, '-------- start test_argwhere_c1s --------'
        s1 = 'hello, im fine, thank you and you'


        call argwhere_c1s(s1, 'o', iaa1)

        print *, iaa1
        call assert(all(iaa1 .eq. [5, 24, 32]), 'Error in string/argwhere_c1s')

        print *, '-------- pass test_argwhere_c1s --------'
        print *, ''
    end subroutine

    subroutine test_toString()
        implicit none
        character(80) :: s1

        print *, ''
        print *, '-------- start test_toString --------'
        s1 = 'now'

        print *, toString('what', 1, 3.14, 'now', cmplx(1,2), 'wow', nSpace=1)
        call assert(toString('now','you') .eq. 'nowyou', 'Error in rdee_string/toString, test 1')
        call assert(toString(s1, 1, 'you', nSpace=1) .eq. 'now 1 you', 'Error in rdee_string/toString, test 2')
        print *, '-------- pass test_toString --------'
        print *, ''
    end subroutine

    subroutine test_string1dBuilder()
        implicit none
        
        print *, ''
        print *, ''
        print *, '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        print *, '>>> start test_string1dBuilder'
        print *, '>>> assume success without compiling and running errors'
        print *, '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
        call pri_(string1dBuilder('how', ' ', 'about', ' ', 'you', '!', ''))
        print *, '-------- pass test_string1dBuilder --------'
        print  *, ''

    end subroutine

    subroutine pri_(sa1)
        implicit none
        character(*), intent(in) :: sa1(:)

        print *, sa1
    end subroutine

End Program
