Module rdee_base
    Implicit none

Contains
    Subroutine assert(bval, msg)
        Implicit none
        Logical, Intent(in) :: bval
        Character(*), Intent(in) :: msg
        
        If (.not. bval) then
            write(*,*) msg
            Stop 999
        Endif

    End Subroutine assert


End Module