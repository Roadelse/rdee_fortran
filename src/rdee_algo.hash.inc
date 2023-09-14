
Function hash_mod_is(key, size) result(rst_i)
    implicit none
    ! ............................ Argument
    class(*), intent(in) :: key
    integer, intent(in) :: size
    ! ............................ return variable
    integer :: rst_i

    select type(k => key)
        type is (integer)
            rst_i = hash_mod_i(k, size)
        type is (character(*))
            rst_i = hash_mod_s(k, size)
        class default
            stop 'Error in rdee_algo/hash/hash_mod, this function only support int or character by now'
    end select

End Function


Function hash_mod_i(key, size) result(rst_i)
    implicit none
    ! ............................ Argument
    integer, intent(in) :: key, size
    ! ............................ return variable
    integer :: rst_i

    rst_i = mod(key, size) + 1

end Function
     
Function hash_mod_s(key, size) result(rst_i)
    implicit none
    ! ............................ Argument
    character(*), intent(in) :: key
    integer, intent(in) :: size
    ! ............................ return variable
    integer :: rst_i
    ! ............................ local variables
    character :: tmp(len(key))
    integer :: keySum
    integer :: i

    do i = 1, len_trim(key)
       tmp(i) = key(i:i)
    end do
    keySum = SUM(ICHAR(tmp))

    rst_i = mod(keySum, size) + 1

end Function