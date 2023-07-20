Module rdee_array
    implicit none


Contains
    Function ispan(i1, i2, stride_) result(rst)
        implicit none
        ! ............................. Arguments
        integer, intent(in) :: i1, i2
        integer, intent(in), optional :: stride_
        ! ............................. Local variables
        integer, allocatable :: rst(:)
        integer :: N, i, stride
        ! ............................. Main body
        if (present(stride_)) then
            stride = stride_
        else
            stride = 1
        end if
        N = (i2 - i1) / stride + 1
        allocate(rst(N))
        do i = 1, N
            rst(i) = i1 + (i-1) * stride
        end do

        return
    End Function

    Function fspan(f1, f2, npts) result(rst)
        implicit none
        ! ............................. Arguments
        real(kind=4), intent(in) :: f1, f2
        integer, intent(in) :: npts
        ! ............................. Return variable
        real(kind=4) :: rst(npts)
        ! ............................. Local Variables
        real(kind=4) :: step
        integer :: i

        ! ............................. Main body
        step = (f2 - f1) / (npts - 1)
        do i = 1, npts
            rst(i) = f1 + (i-1) * step
        end do
        return
    End Function

End Module

