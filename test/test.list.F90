program main
use rdee_ds
implicit none
intrinsic :: loc
type(list) :: L1, L2
type(node) :: n1, n2
integer :: i,j,k
integer, allocatable :: iaa1(:)
real(kind=4) :: r1
character(5) :: s5
character(80) :: s80

print *, '--------- (A) now test constructor & get for v ----------'
print *, 'list test A1. should be: item = 1; 1'
L1 = list(1)
call L1%head%print
call L1%get(1, i)
print *, i
call assert(i .eq. 1, 'Error in rdee_ds/list test A1')
print *, ''


print *, 'list test A2. should be : 2'
L1 = list([1,2,3])
call L1%get(2, i)
print *, i
call assert(i .eq. 2, 'Error in rdee_ds/list test A2')
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

end program

