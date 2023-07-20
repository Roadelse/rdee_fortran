Module rdee_array
    use rdee_ds
    implicit none

    Interface remove_val
        Module Procedure remove_val_int4
        Module Procedure remove_val_int8
        Module Procedure remove_val_float
        Module Procedure remove_val_double
        Module Procedure remove_val_logical
        Module Procedure remove_val_string
    End Interface


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

    
    Subroutine remove_val_int4(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=4), intent(in) :: arr(:)
        integer(kind=4), intent(in) :: val
        integer(kind=4), allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_int4

    Subroutine remove_val_int8(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        integer(kind=8), intent(in) :: arr(:)
        integer(kind=8), intent(in) :: val
        integer(kind=8), allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_int8

    Subroutine remove_val_float(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=4), intent(in) :: arr(:)
        real(kind=4), intent(in) :: val
        real(kind=4), allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_float

    Subroutine remove_val_double(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        real(kind=8), intent(in) :: arr(:)
        real(kind=8), intent(in) :: val
        real(kind=8), allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_double

    Subroutine remove_val_logical(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        logical, intent(in) :: arr(:)
        logical, intent(in) :: val
        logical, allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_logical

    Subroutine remove_val_string(arr, val, rst, maxCount_, rev_)
        implicit none
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Arguments
        character(*), intent(in) :: arr(:)
        character(*), intent(in) :: val
        character(*), allocatable, intent(out) :: rst(:)
        integer, intent(in), optional :: maxCount_ ! how many elements 
        integer, intent(in), optional :: rev_    ! 0 or not

        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Local variables
        integer :: maxCount,  rev
        integer :: i, j
        integer :: count, stride, st, ed
        integer :: idx_holder(size(arr))
        ! integer, allocatable :: idxs(:)
        ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> main body
        maxCount = 1
        if (present(maxCount_)) maxCount = maxCount_

        rev = 0
        if (present(rev_)) rev = rev_


        if (rev .eq. 0) then
            stride = 1
            st = 1
            ed = size(arr)            
        else
            stride = -1
            ed = 1
            st = size(arr)    
        end if

        count = 0
        ! L = list()

        j = 0
        do i = st, ed, stride
            if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
                count = count + 1
            else
                ! call L%append(i)
                j = j + 1
                idx_holder(j) = i
            end if
        end do  


        ! if (L%size .eq. 0) return
        if (j .eq. 0) return

        ! if (rev .ne. 0) call L%reverse
        if (rev .ne. 0) idx_holder(1:j) = idx_holder(j:1:-1)

        ! call L%toArray(idxs)
        allocate(rst, source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_string

End Module
