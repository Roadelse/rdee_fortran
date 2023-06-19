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

    elemental integer function eq_um(x, y) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: x, y

        select type(xp => x)
            type is (integer(kind=4))
                select type(yp => y)
                    type is (integer(kind=4))
                        rst = 0
                        if (xp .eq. yp) rst = 1
                    class default
                        rst = -1
                end select
            type is (integer(kind=8))
                select type(yp => y)
                    type is (integer(kind=8))
                        rst = 0
                        if (xp .eq. yp) rst = 1
                    class default
                        rst = -1
                end select
            type is (real(kind=4))
                select type(yp => y)
                    type is (real(kind=4))
                        rst = 0
                        if (xp .eq. yp) rst = 1
                    class default
                        rst = -1
                end select
            type is (real(kind=8))
                select type(yp => y)
                    type is (real(kind=8))
                        rst = 0
                        if (xp .eq. yp) rst = 1
                    class default
                        rst = -1
                end select
            type is (logical)
                select type(yp => y)
                    type is (logical)
                        rst = 0
                        if (xp .eqv. yp) rst = 1
                    class default
                        rst = -1
                end select
            type is (character(*))
                select type(yp => y)
                    type is (character(*))
                        rst = 0
                        if (xp .eq. yp) rst = 1
                    class default
                        rst = -1
                end select
            class default
                rst = -2
        end select
        return
    end function

    
    integer function eq_1d_um(x, y) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: x(:), y(:)

        select type(xp => x)
            type is (integer(kind=4))
                select type(yp => y)
                    type is (integer(kind=4))
                        rst = 0
                        if (all(xp .eq. yp)) rst = 1
                    class default
                        rst = -1
                end select
            type is (integer(kind=8))
                select type(yp => y)
                    type is (integer(kind=8))
                        rst = 0
                        if (all(xp .eq. yp)) rst = 1
                    class default
                        rst = -1
                end select
            type is (real(kind=4))
                select type(yp => y)
                    type is (real(kind=4))
                        rst = 0
                        if (all(xp .eq. yp)) rst = 1
                    class default
                        rst = -1
                end select
            type is (real(kind=8))
                select type(yp => y)
                    type is (real(kind=8))
                        rst = 0
                        if (all(xp .eq. yp)) rst = 1
                    class default
                        rst = -1
                end select
            type is (logical)
                select type(yp => y)
                    type is (logical)
                        rst = 0
                        if (all(xp .eqv. yp)) rst = 1
                    class default
                        rst = -1
                end select
            type is (character(*))
                select type(yp => y)
                    type is (character(*))
                        rst = 0
                        if (all(xp .eq. yp)) rst = 1
                    class default
                        rst = -1
                end select
            class default
                rst = -2
        end select
        return
    end function

End Module