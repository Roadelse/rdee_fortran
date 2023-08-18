program main
    use rdee_ds

    implicit none

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
    
    print *, 'should be 3: ', map1%hash(3)
    print *, 'should be 1: ', map2%hash(21)
    print *, 'should be 8: ', map3%hash('wow')
    print *, 'should be 13: ', map4%hash('aha')
    call assert(map1%hash(3) .eq. 3, 'Error in rdee_ds/dict test A1')
    call assert(map4%hash('aha') .eq. 13, 'Error in rdee_ds/dict test A1')

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
    call assert(map1%hasKey(7), 'Error in rdee_dict test D1')
    call assert(.not. map1%hasKey('aha'), 'Error in rdee_dict test D1')


end program
