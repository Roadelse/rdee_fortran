program main
use rdee_base
implicit none

print *, 'should be 0: ', comp_um_scalar(2, 3)
print *, 'should be 1: ', comp_um_scalar(2, 2)
print *, 'should be -1 : ', comp_um_scalar(2, 'hello')
print *, 'should be 1: ', comp_um_scalar('hello', 'hello')
print *, 'should be 1: ', comp_um_scalar(.true., .true.)
print *, 'shuold be -1: ',comp_um_scalar(.true., 3)
print *, 'shuold be 1: ',comp_um_scalar(3.12_4, 3.12_4)


end program
