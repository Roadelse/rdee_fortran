
impure elemental subroutine dict_destructor(this)
    implicit none
    ! ............................ Argument
    type(dict), intent(inout) :: this

    if (allocated(this%lists)) deallocate(this%lists) 
    this%size = 0
    this%hashFunc => null()
end subroutine

function dict_constructor(hash, L) result(inst)
    implicit none
    ! ............................ Argument
    procedure(hash_A2i), optional :: hash
    integer, intent(in), optional :: L
    ! ............................ Return variables
    type(dict) :: inst
    ! ............................ Local variables

    if (present(hash)) then
        inst%hashFunc => hash
    else
        inst%hashFunc => hash_mod_is
    end if

    if (present(L)) then
        inst%size = L
    else
        inst%size = 31
    end if
    allocate(inst%lists(inst%size))

end function

subroutine dict_reset(this)
    implicit none
    ! ............................ Arguments
    class(dict), intent(inout) :: this
    ! ............................ Local variables
    integer :: i

    ! ............................ main body
    if (allocated(this%lists)) then
        do i = 1, this%size
            call this%lists(i)%reset
        end do
        deallocate(this%lists)
    end if

    this%size = 0
    this%hashFunc => null()
end subroutine


! *********************************************************
! 
! *********************************************************
function dict_hash(this, key) result(rst_idx)
    implicit none
    ! ............................ Arguments
    class(dict), intent(in) :: this
    class(*), intent(in) :: key
    ! ............................ result variables
    integer :: rst_idx

    ! ............................ main body
    rst_idx = this%hashFunc(key, this%size)

end function

subroutine dict_print(this, key)
    implicit none
    ! ............................ Arguments
    class(dict), intent(in) :: this
    class(*), intent(in), optional :: key
    ! ............................ local variable
    type(node) :: nodeT, n2
    type(list) :: L
    integer :: i
    ! ............................ main body

    L = this%keys()

    if (present(key)) then
        call this%get(key, nodeT)
        call nodeT%print
    else
        do i = 1, L%size
            n2 = L%fget_node(i)
            call this%get(n2%item, nodeT)
            call nodeT%print
        end do
    end if
end subroutine dict_print


function dict_keys(this) result(rstL)
    implicit none
    ! ............................  Arguments
    class(dict), intent(in) :: this
    ! ............................  local variables
    type(list) :: rstL
    integer :: i

    ! ............................  main body
    if (this%size .eq. 0) return

    do i = 1, this%size
        if (this%lists(i)%size .gt. 0) then
            call rstL%extend(this%lists(i)%keys())
        end if
    end do
end function

subroutine dict_update(this, M)
    implicit none
    ! ............................ Arguments
    class(dict), intent(inout) :: this
    type(dict), intent(in) :: M
    ! ............................ main body
    type(list) :: L
    type(node) :: N
    type(node), pointer :: p

    L = M%keys()
    p => L%head
    do while (associated(p))
        ! call p%print
        call M%get(p%item, N)
        ! print *, 'cp2'
        ! CALL N%print
        call this%set(N)
        p => p%next
    end do

end subroutine dict_update




! *********************************************************
! set & get procedures for dict
! *********************************************************
subroutine dict_set_node(this, n)
    implicit none
    ! ............................ Argument
    class(dict), intent(inout) :: this
    type(node), intent(in) :: n
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(n%key)

    listTar = this%lists(hashVal)
    p => this%lists(hashVal)%head
    do while (associated(p))
        if (p%hasKey(n%key)) then
            call p%set(n)
            return
        end if
        p => p%next
    end do

    call this%lists(hashVal)%append(n)  ! maybe add a function to implement shallow copy ...

    p => null()
End Subroutine dict_set_node

subroutine dict_set(this, k, v)
    implicit none
    ! ............................ Argument
    class(dict), intent(inout) :: this
    class(*), intent(in) :: k, v

    call this%set(node(k, v))
    ! ............................ local variable
    ! type(node), pointer :: p
    ! type(list) :: listTar
    ! integer :: hashVal

    ! ! ............................ main body
    ! hashVal = this%hash(k)

    ! listTar = this%lists(hashVal)
    ! p => this%lists(hashVal)%head
    ! do while (associated(p))
    !     if (p%hasKey(k)) then
    !         call p%set(v)
    !         return
    !     end if
    !     p => p%next
    ! end do

    ! allocate(p, source=node(k, v))
    ! call this%lists(hashVal)%append(p)  ! maybe add a function to implement shallow copy ...

    ! p => null()
End Subroutine dict_set

subroutine dict_set_a1d(this, k, v)
    implicit none
    ! ............................ Argument
    class(dict), intent(inout) :: this
    class(*), intent(in) :: k, v(:)
    ! ............................ local variable
    call this%set(node(k, v))
    ! type(node), pointer :: p
    ! type(list) :: listTar
    ! integer :: hashVal

    ! ! ............................ main body
    ! hashVal = this%hash(k)

    ! listTar = this%lists(hashVal)
    ! p => this%lists(hashVal)%head
    ! do while (associated(p))
    !     if (p%hasKey(k)) then
    !         call p%set(v)
    !         return
    !     end if
    !     p => p%next
    ! end do

    ! allocate(p, source=node(k, v))
    ! call this%lists(hashVal)%append(p)  ! maybe add a function to implement shallow copy ...

    ! p => null()
End Subroutine dict_set_a1d


Function dict_hasKey(this, k) result(rst)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal
    ! ............................ Return variable
    logical :: rst
    ! ............................ main body
    if (this%size .eq. 0) then
        rst = .False.
        return
    end if

    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    if (.not. associated(listTar%head)) then
        rst = .False.
        return
    end if

    p => listTar%head
    do while (associated(p))
        if (p%hasKey(k)) then
            rst = .true.
            return
        end if
        p => p%next
    end do

    rst = .False.
    return
End Function dict_hasKey


subroutine dict_p2node(this, k, np)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    type(node), pointer, intent(inout) :: np
    ! ............................ local variable
    type(list) :: listTar
    integer :: hashVal
    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    np => listTar%head
    
    do while (associated(np))
        if (np%hasKey(k)) then
            return
        end if
        np => np%next
    end do

    return
End Subroutine dict_p2node


function dict_fp2node(this, k) result(np)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    type(node), pointer :: np
    ! ............................ local variable
    type(list) :: listTar
    integer :: hashVal
    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    np => listTar%head
    
    do while (associated(np))
        if (np%hasKey(k)) then
            return
        end if
        np => np%next
    end do

    return
End Function dict_fp2node


subroutine dict_get(this, k, v)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    class(*), intent(out) :: v
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal
    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head
    
    do while (associated(p))
        if (p%hasKey(k)) then
            select type (vp => v)
                type is (node)
                    vp = p
                class default
                    call p%get(v)
            end select
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'
End Subroutine




Subroutine dict_get1d_int4(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=4), allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_int4

Function dict_fget1d_int4(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=4), allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_int4

Function dict_fget_int4(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=4) :: v

    call this%get(k, v)
End Function dict_fget_int4
Subroutine dict_get1d_int8(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=8), allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_int8

Function dict_fget1d_int8(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=8), allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_int8

Function dict_fget_int8(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    integer(kind=8) :: v

    call this%get(k, v)
End Function dict_fget_int8
Subroutine dict_get1d_real4(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=4), allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_real4

Function dict_fget1d_real4(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=4), allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_real4

Function dict_fget_real4(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=4) :: v

    call this%get(k, v)
End Function dict_fget_real4
Subroutine dict_get1d_real8(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=8), allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_real8

Function dict_fget1d_real8(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=8), allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_real8

Function dict_fget_real8(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    real(kind=8) :: v

    call this%get(k, v)
End Function dict_fget_real8
Subroutine dict_get1d_logical(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    logical, allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_logical

Function dict_fget1d_logical(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    logical, allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_logical

Function dict_fget_logical(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    logical :: v

    call this%get(k, v)
End Function dict_fget_logical
Subroutine dict_get1d_string(this, k, v1d)
    implicit none
    ! ............................ Argument
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    character(*), allocatable, intent(out) :: v1d(:)
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal

    ! ............................ main body
    hashVal = this%hash(k)
    listTar = this%lists(hashVal)
    p => listTar%head

    do while (associated(p))
        if (p%hasKey(k)) then
            call p%get(v1d)
            return
        end if
        p => p%next
    end do

    stop 'Error in rdee_ds/dict/get, cannot find target key'

End Subroutine dict_get1d_string

Function dict_fget1d_string(this, k) result(v1d)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    character(len=256), allocatable :: v1d(:)
    call this%get(k, v1d)
End Function dict_fget1d_string

Function dict_fget_string(this, k) result(v)
    implicit none
    ! ............................ Argument & return variable
    class(dict), intent(in) :: this
    class(*), intent(in) :: k
    character(len=256) :: v

    call this%get(k, v)
End Function dict_fget_string



subroutine dict_del(this, k)
    implicit none
    ! ............................ Argument
    class(dict), intent(inout) :: this
    class(*), intent(in) :: k
    ! ............................ local variable
    type(node), pointer :: p
    type(list) :: listTar
    integer :: hashVal
    ! ............................ main body
    hashVal = this%hash(k)

    listTar = this%lists(hashVal)
    call listTar%popKey(k)
End Subroutine

