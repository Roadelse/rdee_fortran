program main
use rdee_ds
implicit none
    call test_basic
    call test_reverse

Contains

    Subroutine test_basic()
        implicit none

        intrinsic :: loc
        type(list) :: L1, L2
        type(node) :: n1, n2
        integer :: i,j,k
        integer, allocatable :: iaa1(:)
        integer :: ia1(3)

        real(kind=4) :: r1
        character(5) :: s5
        character(80) :: s80
        character(80), allocatable :: saa1(:)

        print *, '--------- (A) now test constructor & get for v ----------'
        print *, 'list test A1. should be: item = 1; 1'
        L1 = list(1)
        call L1%head%print
        call L1%get(1, i)
        print *, i
        call assert(i .eq. 1, 'Error in rdee_ds/list test A1')
        print *, ''


        print *, 'list test A2. should be : 2, 3'
        L1 = list([1,2,3])
        call L1%get(2, i)
        call L1%get(3, ia1(2))
        print *, i, ia1(2)
        call assert(i .eq. 2, 'Error in rdee_ds/list test A2')
        call assert(ia1(2) .eq. 3, 'Error in rdee_ds/lit test A2')
        print *, ''

        print *, '--------- (B) now test append & insert ----------'
        print *, 'list test B1. should be : item = hello; item1d = 1 2 3; hello; 1 2 3'
        L1 = list()
        call L1%append(23.4)
        call L1%insert(1, 'hello')
        call L1%insert(3, [1,2,3])
        call L1%head%print
        call L1%tail%print
        call L1%get(1, s5)
        call L1%get(3, iaa1)
        print *, s5
        print *, iaa1
        call assert(all(iaa1 == [1,2,3]), 'Error in rdee_ds/list test B1')
        deallocate(iaa1)
        print *, ''

        print *, '--------- (C) now test pop/del/index ----------'
        L1 = list([1,2,3,4])
        call L1%append([1.2, 2.3])
        call L1%insert(2, .true.)
        call L1%pop(1)
        call L1%del(3)
        call L1 %print
        print *, 'L1%size, should be 4: ', L1%size
        print *, 'should be : T; 1.2 2.3'
        call L1%head%print
        call L1%tail%print
        print *, 'L1%index([1.2, 2.3]), should be 4: ', L1%index([1.2, 2.3])
        call assert(L1%index([1.2, 2.3]) .eq. 4, 'Error in rdee_ds/list test C1')

        print *, '--------- (D) now test toArray ----------'
        L1 = list([1,2,3,4])
        call L1%toArray(iaa1)
        print *, iaa1
        call assert(all(iaa1 .eq. [1,2,3,4]), 'Error in rdee_ds/list test D1')

        L1 = list()
        call L1%append('hello')
        call L1%append('nowwhat')
        call L1%toArray(saa1)
        print *, saa1
    End Subroutine

    Subroutine test_reverse()
        type(list) :: L1, L2

        L1 = list()
        call L1%append(1)
        call L1%append(2)
        call L1%append(3)
        call L1%print
        call L1%head%print
        call L1%tail%print
        call assert(L1%head .eq. 1, 'Error in list%reverse')
        call assert(L1%tail .eq. 3, 'Error in list%reverse')
        
        print *, associated(L1%head%prev)
        print *, associated(L1%tail%next)
        call assert(.not. associated(L1%head%prev), 'Error in list%reverse')
        call assert(.not. associated(L1%tail%next), 'Error in list%reverse')


        call list_reverse(L1)
        print *, '--------------'
        call L1%print
        call L1%head%print
        call L1%tail%print
        call assert(L1%head .eq. 3, 'Error in list%reverse')
        call assert(L1%tail .eq. 1, 'Error in list%reverse')

        call L1%insert(1, 'hello')
        call list_reverse(L1)
        print *, '--------------'
        call L1%print
        call L1%head%print
        call L1%tail%print
        call assert(L1%head .eq. 1, 'Error in list%reverse')
        call assert(L1%tail .eq. 'hello', 'Error in list%reverse')

        print *, associated(L1%head%prev)
        print *, associated(L1%tail%next)
        call assert(.not. associated(L1%head%prev), 'Error in list%reverse')
        call assert(.not. associated(L1%tail%next), 'Error in list%reverse')

    End Subroutine

end program

