Module rdee_string
  use rdee_ds
  implicit none

Contains

! *********************************************************
! This subroutine aims to get indexes for specific char in string
! *********************************************************
subroutine argwhere_c1s(S, c, idxs)  ! char in(1 as homophonic) string
  implicit none
  ! ......................... Arguments
  character(len=*), intent(in) :: S
  character, intent(in) :: c
  integer(kind=4), allocatable, intent(out) :: idxs(:)
  ! ......................... Local variables
  type(list) :: rst_list
  integer :: i

  ! ......................... main body
  rst_list = list()
  do i = 1, len_trim(S)
    if (S(i:i) .eq. c) then
      call rst_list%append(i)
    end if
  end do

  call rst_list%toArray(idxs)

  return

end subroutine argwhere_c1s



! *********************************************************
! This subroutine aims to convert all generic data type into string
! *********************************************************
function a2s(A, L_) result(S)
    implicit none
    ! ...................................... Arguments
    class(*), intent(in) :: A
    integer, intent(in), optional :: L_
    ! ...................................... return variable
    character(len=:),allocatable :: S
    ! ...................................... local variable
    integer :: L

    ! ...................................... main body
    if (present(L_)) then
        L = L_
    else
        L = -1
    end if

    select type(A)
        type (integer(kind=4))
          
          write(strT, '(I0)') type_info(2,j)



end subroutine


End Module