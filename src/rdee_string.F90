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


End Module