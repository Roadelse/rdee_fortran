Module rdee_array
    ! use rdee_ds
    implicit none

    Interface remove_val
        Module Procedure remove_val_int4
        Module Procedure remove_val_int8
        Module Procedure remove_val_float
        Module Procedure remove_val_double
        Module Procedure remove_val_logical
        Module Procedure remove_val_string
    End Interface

    Interface union_arr1d
        Module Procedure union_arr1d_int4
        Module Procedure union_arr1d_int8
        Module Procedure union_arr1d_float
        Module Procedure union_arr1d_double
        Module Procedure union_arr1d_logical
    End Interface


Contains
Function ispan(i1, i2, stride) result(rst)
    implicit none
    ! ............................. Arguments
    integer, intent(in) :: i1, i2
    integer, intent(in), optional :: stride
    ! ............................. Local variables
    integer, allocatable :: rst(:)
    integer :: N, i, stride_
    ! ............................. Main body
    if (present(stride)) then
        stride_ = stride
    else
        stride_ = 1
    end if
    N = (i2 - i1) / stride_ + 1
    allocate(rst(N))
    do i = 1, N
        rst(i) = i1 + (i-1) * stride_
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


! *********************************************************
! array querying apis
! *********************************************************
Function ind(la) result(rst)
    !!! return indexes for true elements in la
    implicit none
    logical, intent(in) :: la(:)
    integer(kind=4), allocatable :: rst(:)

    integer(kind=4) :: i, j
    integer(kind=4) :: rstBuffer(size(la))

    ! ...................................... main body
    j = 1
    do i = 1, size(la)
        if (la(i)) then
            rstBuffer(j) = i
            j = j + 1
        end if
    end do
    rst = rstBuffer(1:j-1)
End Function ind



    
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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eq. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eq. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eq. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eq. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eqv. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

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
            ! if (count .ne. maxCount .and. eq_um(arr(i), val) .eq. 1) then
            if (count .ne. maxCount .and. arr(i) .eq. val) then
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
        allocate(rst(j), source=arr(idx_holder(1:j)))

        return
    End Subroutine remove_val_string

    Function union_arr1d_int4(a1, a2, a3, a4, a5, a6, a7) result(rst)
        implicit none
        ! ....................................... Arguments
        integer(kind=4), intent(in) :: a1(:), a2(:)
        integer(kind=4), intent(in), optional :: a3(:),a4(:),a5(:),a6(:),a7(:)
        ! ....................................... return variable
        integer(kind=4), allocatable :: rst(:)
        integer :: S, i
        ! ....................................... Main body
        S = size(a1) + size(a2)
        if (present(a3)) S = S + size(a3)
        if (present(a4)) S = S + size(a4)
        if (present(a5)) S = S + size(a5)
        if (present(a6)) S = S + size(a6)
        if (present(a7)) S = S + size(a7)

        allocate(rst(S))
        rst(1:size(a1)) = a1
        rst(size(a1) + 1:size(a1)+size(a2)) = a2
        if (present(a3)) rst( &
            size(a1)+size(a2)+1 : &
            size(a1)+size(a2)+size(a3)) = a3
        if (present(a4)) rst( &
            size(a1)+size(a2)+size(a3)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)) = a4
        if (present(a5)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)) = a5
        if (present(a6)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)) = a6
        if (present(a7)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+size(a7)) = a7
        
        return
    End Function
    Function union_arr1d_int8(a1, a2, a3, a4, a5, a6, a7) result(rst)
        implicit none
        ! ....................................... Arguments
        integer(kind=8), intent(in) :: a1(:), a2(:)
        integer(kind=8), intent(in), optional :: a3(:),a4(:),a5(:),a6(:),a7(:)
        ! ....................................... return variable
        integer(kind=8), allocatable :: rst(:)
        integer :: S, i
        ! ....................................... Main body
        S = size(a1) + size(a2)
        if (present(a3)) S = S + size(a3)
        if (present(a4)) S = S + size(a4)
        if (present(a5)) S = S + size(a5)
        if (present(a6)) S = S + size(a6)
        if (present(a7)) S = S + size(a7)

        allocate(rst(S))
        rst(1:size(a1)) = a1
        rst(size(a1) + 1:size(a1)+size(a2)) = a2
        if (present(a3)) rst( &
            size(a1)+size(a2)+1 : &
            size(a1)+size(a2)+size(a3)) = a3
        if (present(a4)) rst( &
            size(a1)+size(a2)+size(a3)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)) = a4
        if (present(a5)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)) = a5
        if (present(a6)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)) = a6
        if (present(a7)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+size(a7)) = a7
        
        return
    End Function
    Function union_arr1d_float(a1, a2, a3, a4, a5, a6, a7) result(rst)
        implicit none
        ! ....................................... Arguments
        real(kind=4), intent(in) :: a1(:), a2(:)
        real(kind=4), intent(in), optional :: a3(:),a4(:),a5(:),a6(:),a7(:)
        ! ....................................... return variable
        real(kind=4), allocatable :: rst(:)
        integer :: S, i
        ! ....................................... Main body
        S = size(a1) + size(a2)
        if (present(a3)) S = S + size(a3)
        if (present(a4)) S = S + size(a4)
        if (present(a5)) S = S + size(a5)
        if (present(a6)) S = S + size(a6)
        if (present(a7)) S = S + size(a7)

        allocate(rst(S))
        rst(1:size(a1)) = a1
        rst(size(a1) + 1:size(a1)+size(a2)) = a2
        if (present(a3)) rst( &
            size(a1)+size(a2)+1 : &
            size(a1)+size(a2)+size(a3)) = a3
        if (present(a4)) rst( &
            size(a1)+size(a2)+size(a3)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)) = a4
        if (present(a5)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)) = a5
        if (present(a6)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)) = a6
        if (present(a7)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+size(a7)) = a7
        
        return
    End Function
    Function union_arr1d_double(a1, a2, a3, a4, a5, a6, a7) result(rst)
        implicit none
        ! ....................................... Arguments
        real(kind=8), intent(in) :: a1(:), a2(:)
        real(kind=8), intent(in), optional :: a3(:),a4(:),a5(:),a6(:),a7(:)
        ! ....................................... return variable
        real(kind=8), allocatable :: rst(:)
        integer :: S, i
        ! ....................................... Main body
        S = size(a1) + size(a2)
        if (present(a3)) S = S + size(a3)
        if (present(a4)) S = S + size(a4)
        if (present(a5)) S = S + size(a5)
        if (present(a6)) S = S + size(a6)
        if (present(a7)) S = S + size(a7)

        allocate(rst(S))
        rst(1:size(a1)) = a1
        rst(size(a1) + 1:size(a1)+size(a2)) = a2
        if (present(a3)) rst( &
            size(a1)+size(a2)+1 : &
            size(a1)+size(a2)+size(a3)) = a3
        if (present(a4)) rst( &
            size(a1)+size(a2)+size(a3)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)) = a4
        if (present(a5)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)) = a5
        if (present(a6)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)) = a6
        if (present(a7)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+size(a7)) = a7
        
        return
    End Function
    Function union_arr1d_logical(a1, a2, a3, a4, a5, a6, a7) result(rst)
        implicit none
        ! ....................................... Arguments
        logical, intent(in) :: a1(:), a2(:)
        logical, intent(in), optional :: a3(:),a4(:),a5(:),a6(:),a7(:)
        ! ....................................... return variable
        logical, allocatable :: rst(:)
        integer :: S, i
        ! ....................................... Main body
        S = size(a1) + size(a2)
        if (present(a3)) S = S + size(a3)
        if (present(a4)) S = S + size(a4)
        if (present(a5)) S = S + size(a5)
        if (present(a6)) S = S + size(a6)
        if (present(a7)) S = S + size(a7)

        allocate(rst(S))
        rst(1:size(a1)) = a1
        rst(size(a1) + 1:size(a1)+size(a2)) = a2
        if (present(a3)) rst( &
            size(a1)+size(a2)+1 : &
            size(a1)+size(a2)+size(a3)) = a3
        if (present(a4)) rst( &
            size(a1)+size(a2)+size(a3)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)) = a4
        if (present(a5)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)) = a5
        if (present(a6)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)) = a6
        if (present(a7)) rst( &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+1 : &
            size(a1)+size(a2)+size(a3)+size(a4)+size(a5)+size(a6)+size(a7)) = a7
        
        return
    End Function
End Module
