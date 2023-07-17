pure function node_constructor() result(inst)
    ! ............................ return variable
    type(node) :: inst
    inst%style = 0  ! for avoidance of compilation warning unused variable
    return
end function
function node_constructor_v(v) result(inst)
    ! ............................ Arguments
    class(*), intent(in) :: v
    ! ............................ return variable
    type(node) :: inst

    select type(vp => v)
        type is (node)
            inst = vp
        class default
            allocate(inst%item, source=v)
    end select
    inst%style = 1
end function
pure function node_constructor_v1d(v) result(inst)
    ! ............................ Arguments
    class(*), intent(in) :: v(:)
    ! ............................ return variable
    type(node) :: inst

    allocate(inst%item1d, source=v)
    inst%style = 2
end function
pure function node_constructor_k_v(k, v) result(inst)
    ! ............................ Arguments
    class(*), intent(in) :: k
    class(*), intent(in) :: v
    ! ............................ return variable
    type(node) :: inst

    allocate(inst%key, source=k)
    allocate(inst%item, source=v)
    inst%style = -1
end function
pure function node_constructor_k_v1d(k, v) result(inst)
    ! ............................ Arguments
    class(*), intent(in) :: k
    class(*), intent(in) :: v(:)
    ! ............................ return variable
    type(node) :: inst

    allocate(inst%key, source=k)
    allocate(inst%item1d, source=v)
    inst%style = -2
end function
impure elemental subroutine node_destructor(this)
    implicit none
    type(node),intent(inout) :: this

    call this%reset
end subroutine
subroutine node_reset(this)
    implicit none
    ! ............................ Arguments
    class(node),intent(inout) :: this

    ! ............................ main body
    if (allocated(this%key)) deallocate(this%key)
    if (allocated(this%item)) deallocate(this%item)
    if (allocated(this%item1d)) deallocate(this%item1d)
    this%next => null()
    this%prev => null()
end subroutine


function eq_node_item_um(n, x) result(rst_b)
    implicit none
    ! ............................ Arguments
    type(node), intent(in) :: n
    class(*), intent(in) :: x
    ! ............................ return variables
    logical :: rst_b
    ! ............................ local variables
    integer :: i, j, k, count

    ! ............................ main body
    select type (xp => x)
        type is (node)
            if (n%style .ne. xp%style) then  ! actully plays a role of checking inited or not
                rst_b = .false.
                return
            end if
            count = 0
            ! >>>>>>>>>>>>>> check key
            if (allocated(n%key) .and. allocated(xp%key)) then
                if (eq_um(n%key, xp%key) .ne. 1) then
                    rst_b = .false.
                    return
                end if
            end if
            if (allocated(n%item) .and. allocated(xp%item)) then
                if (eq_um(n%item, xp%item) .ne. 1) then
                    rst_b = .false.
                    return
                end if
            end if
            if (allocated(n%item1d) .and. allocated(xp%item1d)) then
                if (size(n%item1d) .ne. size(xp%item1d)) then
                    rst_b = .false.
                    return
                elseif (any(eq_um(n%item1d, xp%item1d) .ne. 1)) then
                    rst_b = .false.
                    return
                end if
            end if
            rst_b = .true.
        class default
            rst_b = eq_um(n%item, x) .eq. 1
    end select

end function eq_node_item_um

function eq_node_item1d_um(n, x) result(rst_b)
    implicit none
    ! ............................ Arguments
    type(node), intent(in) :: n
    class(*), intent(in) :: x(:)
    ! ............................ return variables
    logical :: rst_b
    ! ............................ local variables
    integer :: i, j, k, count

    ! ............................ main body
    rst_b = eq_1d_um(n%item1d, x) .eq. 1

end function eq_node_item1d_um



impure elemental subroutine node_assign_from_node(Lnode, Rnode)
    implicit none
    ! ............................ Argument
    type(node), intent(inout) :: Lnode
    type(node), intent(in) :: Rnode

    ! ............................ main body
    if (allocated(Lnode%key)) deallocate(Lnode%key)
    if (allocated(Lnode%item)) deallocate(Lnode%item)
    if (allocated(Lnode%item1d)) deallocate(Lnode%item1d)
    if (associated(Lnode%next)) Lnode%next => null()
    if (associated(Lnode%prev)) Lnode%prev => null()


    if (allocated(Rnode%key)) allocate(Lnode%key, source=Rnode%key)
    if (allocated(Rnode%item)) allocate(Lnode%item, source=Rnode%item)
    if (allocated(Rnode%item1d)) allocate(Lnode%item1d, source=Rnode%item1d)
    if (associated(Rnode%next)) Lnode%next => Rnode%next
    if (associated(Rnode%prev)) Lnode%prev => Rnode%prev

    Lnode%style = Rnode%style

end subroutine

subroutine node_print(this)
    implicit none
    ! ............................ Argument
    class(node), intent(in) :: this

    ! ............................ main body
    if (allocated(this%key)) call node_print_key(this)
    if (allocated(this%item)) call node_print_item(this)
    if (allocated(this%item1d)) call node_print_item1d(this)

end subroutine


subroutine node_print_key(this)
    implicit none
    ! ............................ Argument
    type(node), intent(in) :: this
    ! ............................ local variables
    integer :: i

    ! ............................ main body
    select type (x => this%key)
        type is (integer)
            print *, 'key = ',x
        type is (real(kind=4))
            print *, 'key = ',x
        type is (real(kind=8))
            print *, 'key = ',x
        type is (logical)
            print *, 'key = ',x
        type is (character(*))
            print *, 'key = ',x
        class default
            stop 'Error in rdee_ds/node/print_key, unknown data type'
    end select
end subroutine node_print_key
subroutine node_print_item(this)
    implicit none
    ! ............................ Argument
    type(node), intent(in) :: this
    ! ............................ local variables
    integer :: i

    ! ............................ main body
    select type (x => this%item)
        type is (integer)
            print *, 'item = ',x
        type is (real(kind=4))
            print *, 'item = ',x
        type is (real(kind=8))
            print *, 'item = ',x
        type is (logical)
            print *, 'item = ',x
        type is (character(*))
            print *, 'item = ',x
        class default
            stop 'Error in rdee_ds/node/print_item, unknown data type'
    end select
end subroutine node_print_item
subroutine node_print_item1d(this)
    implicit none
    ! ............................ Argument
    type(node), intent(in) :: this
    ! ............................ local variables
    integer :: i

    ! ............................ main body
    select type (x => this%item1d)
        type is (integer)
            print *, 'item1d = ',x
        type is (real(kind=4))
            print *, 'item1d = ',x
        type is (real(kind=8))
            print *, 'item1d = ',x
        type is (logical)
            print *, 'item1d = ',x
        type is (character(*))
            print *, 'item1d = ',(x(i), i = 1, size(x))
        class default
            stop 'Error in rdee_ds/node/print_item1d, unknown data type'
    end select
end subroutine node_print_item1d
subroutine node_get_item(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    class(*), intent(out) :: item
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0

    if (allocated(this%item)) then
        select type (v => item)
            type is (integer(kind=4))
                select type (v2 => this%item)
                    type is (integer(kind=4))
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            type is (integer(kind=8))
                select type (v2 => this%item)
                    type is (integer(kind=8))
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            type is (real(kind=4))
                select type (v2 => this%item)
                    type is (real(kind=4))
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            type is (real(kind=8))
                select type (v2 => this%item)
                    type is (real(kind=8))
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            type is (logical)
                select type (v2 => this%item)
                    type is (logical)
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            type is (character(*))
                select type (v2 => this%item)
                    type is (character(*))
                        v = v2
                    class default
                        istat = 1  ! unmatched data type
                end select
            class default
                istat = 2  ! unknown arg-item data  type
        end select
    else
        istat = 3  ! not allocated
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get'
            end select        
        end if
    elseif (istat .ne. 0) then
        select case (istat)
            case(1)
                print *,'Error in rdee_ds/node/get, unmatched data type'
            case(2)
                print *,'Error in rdee_ds/node/get, unknown arg-item data type'
            case(3)
                print *,'Error in rdee_ds/node/get, not allocated'
        end select
        stop 3
    end if
end subroutine node_get_item


! The belowing subroutine is wrong since I cannot pass a solid data type, allocatable array(:) 
! to a unlimited polymorphic, allocatable dummy argument ...
! subroutine node_get_a1d_um(this, item, stat, errmsg) 
!     implicit none

!     class(node), intent(in) :: this
!     class(*), allocatable, intent(out) :: item(:)
!     integer, intent(out), optional :: stat
!     character(*),intent(out), optional :: errmsg

!     integer :: istat

!     if (allocated(this%item1d)) then
!         allocate(item, source=this%item1d)
!     else
!         istat = 1
!     end if

!     if (present(stat)) then
!         stat = istat
!         if (present(errmsg)) then
!             select case (istat)
!                 case (0)
!                     errmsg = ''
!                 case default
!                     errmsg = 'Error in rdee_ds/node/get'
!             end select        
!         end if
!     elseif (istat .ne. 0) then
!         stop 'Error in rdee_ds/node/get'
!     end if
! end subroutine


subroutine node_get_item1d_int4(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    integer(kind=4), allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (integer(kind=4))
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_int4'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_int4'
    end if
end subroutine
subroutine node_get_item1d_int8(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    integer(kind=8), allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (integer(kind=8))
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_int8'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_int8'
    end if
end subroutine
subroutine node_get_item1d_float(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    real(kind=4), allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (real(kind=4))
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_float'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_float'
    end if
end subroutine
subroutine node_get_item1d_double(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    real(kind=8), allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (real(kind=8))
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_double'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_double'
    end if
end subroutine
subroutine node_get_item1d_logical(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    logical, allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (logical)
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_logical'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_logical'
    end if
end subroutine
subroutine node_get_item1d_string(this, item, stat, errmsg)
    implicit none

    class(node), intent(in) :: this
    character(*), allocatable, intent(out) :: item(:)
    integer, intent(out), optional :: stat
    character(*),intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%item1d)) then
        select type (v => this%item1d)
            type is (character(*))
                allocate(item, source=v)
            class default
                istat = 1  ! unmatched data type
        end select
    else
        istat = 2    ! not allocated item1d
    end if

    if (present(stat)) then
        stat = istat
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case default
                    errmsg = 'Error in rdee_ds/node/get_item1d_string'
            end select        
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/get_item1d_string'
    end if
end subroutine
subroutine node_set_v(this, x)
    implicit none
    ! ............................ Argument
    class(node), intent(inout) :: this
    class(*), intent(in) :: x

    ! ............................ main body
    ! this = node(v)  ! cannot do this because next & prev will also be replaced other than value itself
    if (allocated(this%item)) deallocate(this%item)
    if (allocated(this%item1d)) deallocate(this%item1d)
    select type (xp => x)
        type is (node)
            if (allocated(xp%item)) allocate(this%item, source=xp%item)
            if (allocated(xp%item1d)) allocate(this%item1d, source=xp%item1d)
        class default
            allocate(this%item, source=x)
    end select
    this%style = 1
    if (allocated(this%key))  this%style = -1
end subroutine

subroutine node_set_v1d(this, x)
    implicit none
    ! ............................ Argument
    class(node), intent(inout) :: this
    class(*), intent(in) :: x(:)

    ! ............................ main body
    if (allocated(this%item)) deallocate(this%item)
    if (allocated(this%item1d)) deallocate(this%item1d)

    allocate(this%item1d, source=x)

    this%style = 2
    if (allocated(this%key))  this%style = -2

end subroutine

subroutine node_set_k_v(this, k, x)
    implicit none
    ! ............................ Argument
    class(node), intent(inout) :: this
    class(*), intent(in) :: k
    class(*), intent(in) :: x

    ! ............................ main body
    if (allocated(this%key)) deallocate(this%key)
    allocate(this%key, source=k)
    if (allocated(this%item)) deallocate(this%item)
    if (allocated(this%item1d)) deallocate(this%item1d)
    allocate(this%item, source=x)
    this%style = -1
end subroutine

subroutine node_set_k_v1d(this, k, x)
    implicit none
    ! ............................ Argument
    class(node), intent(inout) :: this
    class(*), intent(in) :: k
    class(*), intent(in) :: x(:)

    ! ............................ main body
    if (allocated(this%key)) deallocate(this%key)
    allocate(this%key, source=k)

    if (allocated(this%item)) deallocate(this%item)
    if (allocated(this%item1d)) deallocate(this%item1d)
    
    allocate(this%item1d, source=x)

    this%style = -2
end subroutine


! *********************************************************
! for key, hasKey  & getKey
! *********************************************************

subroutine node_getKey(this, key, stat, errmsg)
    implicit none
    ! ................... Arguments
    class(node), intent(inout) :: this
    class(*), intent(out) :: key
    integer, intent(out), optional :: stat
    character(*), intent(out), optional :: errmsg

    integer :: istat

    istat = 0
    if (allocated(this%key)) then
        select type(k => key)
            type is (integer(kind=4))
                select type(k2 => this%key)
                    type is (integer(kind=4))
                        k = k2
                    class default
                        istat = 2  
                end select                  
            type is (integer(kind=8))
                select type(k2 => this%key)
                    type is (integer(kind=8))
                        k = k2
                    class default
                        istat = 2  
                end select                  
            type is (real(kind=4))
                select type(k2 => this%key)
                    type is (real(kind=4))
                        k = k2
                    class default
                        istat = 2  
                end select                  
            type is (real(kind=8))
                select type(k2 => this%key)
                    type is (real(kind=8))
                        k = k2
                    class default
                        istat = 2  
                end select                  
            type is (logical)
                select type(k2 => this%key)
                    type is (logical)
                        k = k2
                    class default
                        istat = 2  
                end select                  
            type is (character(*))
                select type(k2 => this%key)
                    type is (character(*))
                        k = k2
                    class default
                        istat = 2  
                end select                  
            class default
                istat = 3
        end select
    else
        istat = 1
    end if

    if (present(stat)) then
        stat = istat
            
        if (present(errmsg)) then
            select case (istat)
                case (0)
                    errmsg = ''
                case (1)
                    errmsg = 'node item is not allocated'
                case (2)
                    errmsg = 'unknown this%key data type'
                case (3)
                    errmsg = 'unknown argument-key data type'
            end select
        end if
    elseif (istat .ne. 0) then
        stop 'Error in rdee_ds/node/getKey, set stat&errmsg for detailed infomation'
    end if

end subroutine node_getKey


function node_hasKey(this, key) result(rst_b)
    implicit none
    ! ................... Arguments
    class(node), Intent(in) :: this
    class(*), intent(in) :: key
    ! ............................ return variable
    logical :: rst_b
    ! ................... main body
    if (.not. allocated(this%key)) then
        rst_b = .false.
        return
    end if

    select type (k => key)
        type is (integer(kind=4))
            select type (k2 => this%key)
                type is (integer(kind=4))
                    rst_b = k .eq. k2
                class default
                    rst_b = .false.
            end select
        type is (integer(kind=8))
            select type (k2 => this%key)
                type is (integer(kind=8))
                    rst_b = k .eq. k2
                class default
                    rst_b = .false.
            end select
        type is (real(kind=4))
            select type (k2 => this%key)
                type is (real(kind=4))
                    rst_b = k .eq. k2
                class default
                    rst_b = .false.
            end select
        type is (real(kind=8))
            select type (k2 => this%key)
                type is (real(kind=8))
                    rst_b = k .eq. k2
                class default
                    rst_b = .false.
            end select
        type is (logical)
            select type (k2 => this%key)
                type is (logical)
                    rst_b = k .eqv. k2
                class default
                    rst_b = .false.
            end select
        type is (character(*))
            select type (k2 => this%key)
                type is (character(*))
                    rst_b = k .eq. k2
                class default
                    rst_b = .false.
            end select
        class default
            rst_b = .false.
    end select

end function