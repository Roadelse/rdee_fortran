program main
    use rdee_ds

    implicit none

    integer :: nargs

    nargs = command_argument_count()

    if (nargs .eq. 0) then
        call test_base
        call test_p2node
    end if

contains

    subroutine test_base()
        type(dict) :: map1, map2, map3, map4
        type(node) :: n1
        type(list) :: L1

        real(kind=8) :: d1
        integer(kind=4),allocatable :: iaa1(:)

        print *, '--------- (A) now test constructor & hash ----------'
        map1 = dict()
        map2 = dict(L=20)
        map3 = dict(hash_mod_is)
        map4 = dict(hash_mod_is, 19)
        
        print *, 'should be 4: ', map1%hash(3)
        print *, 'should be 2: ', map2%hash(21)
        print *, 'should be 9: ', map3%hash('wow')
        print *, 'should be 14: ', map4%hash('aha')
        call assert(map1%hash(3) .eq. 4, 'Error in rdee_ds/dict test A1')
        call assert(map4%hash('aha') .eq. 14, 'Error in rdee_ds/dict test A1')

        call map1%reset
        call map2%reset
        call map3%reset
        call map4%reset

        print *, '--------- (B) now test get & set & del ----------'
        map1 = dict()
        CALL MAP1%set(2, 'hjellop')
        call map1%set(1, [2,3,4])
        call map1%set(3, [4,5,6,7])
        call map1%set(3, .true.)
        L1 = map1%keys()
        print *, 'L1%size, should be 3 : ', L1%size
        call assert(L1%size .eq. 3, 'Error in rdee_dict test B1')
        print *, ''
        call map1%print()

        print *, '--------- (C) now test update & del ----------'
        map2 = dict(L = 3)
        call map2%set(7, 9.9d0)
        print *, 'should be: 1...3...7...'
        call map1%update(map2)
        call map1%del(2)
        call map1%print()

        call map1%get(7, d1)
        call map1%get(1, iaa1)
        call assert(d1 .eq. 9.9d0, 'Error in rdee_dict test C1')
        call assert(all(iaa1 .eq. [2,3,4]), 'Error in rdee_dict test C1')

        print *, '--------- (D) now test hasKey ----------'
        call assert(map1%hasKey(7), 'Error in rdee_dict test D1, A1')
        call map1%set('aha', 3)
        call assert(map1%hasKey('aha'), 'Error in rdee_dict test D1, A2')
        call assert(.not. map1%hasKey('aah'), 'Error in rdee_dict test D1, A3')

        print *, '--------- (E) now test keys ----------'
        L1 = map1%keys()
        call assert(L1%has(7), 'Error in rdee_dict => keys(), Ass-E1')


        PRINT *, '--------- Pass test.dict.F90 => test_base ----------'

    end subroutine

    subroutine test_p2node()
        type(dict) :: map1
        type(node), pointer :: n1, n2
        integer, allocatable :: ia1(:)

        PRINT *, '--------- start test_p2node ----------'

        map1 = dict()
        call map1%set(0, [1,2,3])
        call map1%set('aha', 'nani')

        call map1%p2node(0, n1)

        call n1%print
        call um_assign(n1%item1d(1), 999)

        n2 => map1%fp2node('aha')
        call n2%set(3.0d0)

        call map1%print
        call map1%get(0, ia1)
        call assert(all(ia1 .eq. [999,2,3]), 'Error in test.dict.F90 => test_p2node A1')

        call assert(map1%fget_real8('aha') .eq. 3d0, 'Error in test.dict.F90 => test_p2node A2')

        PRINT *, '--------- Pass test.dict.F90 => test_p2node ----------'

    end subroutine

end program
