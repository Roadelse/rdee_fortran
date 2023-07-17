program test
use rdee_ds
implicit none

type(node) :: n1, n2, n3, n4, n5
type(node), pointer :: pn1, pn2, pn3, pn4, pn5
integer :: i, j, k
integer(kind=4),allocatable :: iaa1(:)
character(5),allocatable :: saa1(:)
real(kind=4) :: r1, r2


print *, '--------- (A) now test constructor & print for v ----------'
n1 = node([1,2,3])
print *, 'node test A1. should be : item1d = 1 2 3'
call n1%print
call assert(n1 == [1,2,3], 'Error in rdee_ds/node test A1')
call n1%reset
print *, ''

n1 = node("hello")
print *, 'node test A2. should be : item = hello'
call n1%print
call assert(n1 == 'hello', 'Error in rdee_ds/node test A2')
call n1%reset
print *, ''

n1 = node(2.34d0)
print *, 'node test A3. should be: item = 2.340000...00'
call n1%print
call assert(n1 == 2.34d0, 'Error in rdee_ds/node test A3')
call n1%reset
print *, ''

print *, 'node test A4. should be: item1d = TF'
n1 = node([.true., .false.])
call n1%print
call assert(n1 == [.true., .false.], 'Error in rdee_ds/node test A4')
call n1%reset
print *, ''

print *, '--------- (B) now test constructor & print for k, v ----------'
n1 = node(v = [1.1,2.2,3.3], k = 'what')
print *, 'node test B1. should be: key = what, item1d = 1.1 2.2 3.3'
call n1%print
call assert(n1 == [1.1, 2.2, 3.3], 'Error in rdee_ds/node test B1')
call assert(n1%hasKey('what'), 'Error in rdee_ds/node test B1')
call n1%reset
print *, ''

n1 = node(4.5d0, 'what')
print *, 'node test B2. should be: key = 4.5; item1d = what'
call n1%print
call assert(n1 == 'what', 'Error in rdee_ds/node test B2')
call assert(n1%hasKey(4.5d0), 'Error in rdee_ds/node test B2')
call n1%reset
print *, ''


print *, '--------- (C) now test get & set ----------'
print *, 'node test C1. should be: item = 9.87 ; 9.87  1'
n1 = node()
call n1%set(9.87)
call n1%print
call n1%node_get_item(r1)
print *, r1, n1%style
call assert(n1 == 9.87, 'Error in rdee_ds/node test C1')
call assert(r1 == 9.87, 'Error in rdee_ds/node test C1')
call n1%reset
print *, ''

print *, 'node test C2. should be: 9 9 9 ; 9 9 9    -2'
n1 = node('pi', [3,1,4])
call n1%set([9,9,9])
call n1%print
call n1%node_get_item1d_int4(iaa1)
print *, iaa1, n1%style
call assert(all(iaa1 == [9,9,9]), 'Error in rdee_ds/node test C2')
deallocate(iaa1)
call n1%reset
print *, ''


print *, 'node test C3. should be: item1d = hellowhat112345 *2   -2'
n1 = node('ts', ['hello', 'what1', '12345'])
call n1%print
call n1%node_get_item1d_string(saa1)
print *, saa1, n1%style
call assert(all(saa1 == ['hello', 'what1', '12345']), 'Error in rdee_ds/node test C3')
deallocate(saa1)
call n1%reset
print *, ''

print *, '--------- (D) now test hasKey & getKey ----------'
print *, 'node test D1. should be: T  2'
n1 = node(2, 3)
print *, n1%hasKey(2)
call n1%getKey(i)
print *, i
call assert(n1%hasKey(2), 'Error in rdee_ds/node test D1')
call assert(i .eq. 2, 'Error in rdee_ds/node test D1')
call n1%reset
print *, ''



end program
