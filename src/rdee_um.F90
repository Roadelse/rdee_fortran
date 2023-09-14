Module rdee_um
    Implicit none

Contains
    Subroutine fum_print(um)
        implicit none
        ! ............................. Argument
        class(*), intent(in) :: um
        ! ............................. main body
        Select Type (ump => um)
            type is (integer(kind=4))
                print *, ump
            type is (integer(kind=8))
                print *, ump
            type is (real(kind=4))
                print *, ump
            type is (real(kind=8))
                print *, ump
            class default
                return
        end select
    end subroutine

    Function um2i4(um) result(rst)
        implicit none
        ! ............................. Arguments & return variable
        class(*), intent(in) :: um
        integer(kind=4) :: rst
        ! ............................. Main body
        select type (ump => um)
            type is (integer(kind=4))
                rst = ump
            type is (integer(kind=8))
                rst = int(ump, 4)
            type is (real(kind=4))
                rst = int(ump, 4)
            type is (real(kind=8))
                rst = int(ump, 4)
            class default
                print *, 'Error! um is not a number'
                stop 2
        end select
    End Function

    Function um2i8(um) result(rst)
        implicit none
        ! ............................. Arguments & return variable
        class(*), intent(in) :: um
        integer(kind=4) :: rst
        ! ............................. Main body
        select type (ump => um)
            type is (integer(kind=4))
                rst = int(ump, 8)
            type is (integer(kind=8))
                rst = ump
            type is (real(kind=4))
                rst = int(ump, 8)
            type is (real(kind=8))
                rst = int(ump, 8)
            class default
                print *, 'Error! um is not a number'
                stop 2
        end select
    End Function
    Function um2r4(um) result(rst)
        implicit none
        ! ............................. Arguments & return variable
        class(*), intent(in) :: um
        real(kind=4) :: rst
        ! ............................. Main body
        select type (ump => um)
            type is (integer(kind=4))
                rst = real(ump, 4)
            type is (integer(kind=8))
                rst = real(ump, 4)
            type is (real(kind=4))
                rst = ump
            type is (real(kind=8))
                rst = real(ump, 4)
            class default
                print *, 'Error! um is not a number'
                stop 2
        end select
    End Function
    Function um2r8(um) result(rst)
        implicit none
        ! ............................. Arguments & return variable
        class(*), intent(in) :: um
        real(kind=8) :: rst
        ! ............................. Main body
        select type (ump => um)
            type is (integer(kind=4))
                rst = real(ump, 8)
            type is (integer(kind=8))
                rst = real(ump, 8)
            type is (real(kind=4))
                rst = real(ump, 8)
            type is (real(kind=8))
                rst = ump
            class default
                print *, 'Error! um is not a number'
                stop 2
        end select
    End Function
    
    Subroutine um_assign(um, v)
        implicit none
        ! ............................. Argument
        class(*), intent(inout) :: um
        class(*), intent(in) :: v
        ! ............................. main body
        Select type(ump => um)
            type is (integer(kind=4))
                select type(vp => v)
                    type is (integer(kind=4))
                        ump = vp
                    type is (integer(kind=8))
                        ump = vp
                    type is (real(kind=4))
                        ump = vp
                    type is (real(kind=8))
                        ump = vp
                    class default
                        print *, 'Error! unmatched data types for um and v'
                        stop 1
                end select
            type is (integer(kind=8))
                select type(vp => v)
                    type is (integer(kind=4))
                        ump = vp
                    type is (integer(kind=8))
                        ump = vp
                    type is (real(kind=4))
                        ump = vp
                    type is (real(kind=8))
                        ump = vp
                    class default
                        print *, 'Error! unmatched data types for um and v'
                        stop 1
                end select
            type is (real(kind=4))
                select type(vp => v)
                    type is (integer(kind=4))
                        ump = vp
                    type is (integer(kind=8))
                        ump = vp
                    type is (real(kind=4))
                        ump = vp
                    type is (real(kind=8))
                        ump = vp
                    class default
                        print *, 'Error! unmatched data types for um and v'
                        stop 1
                end select
            type is (real(kind=8))
                select type(vp => v)
                    type is (integer(kind=4))
                        ump = vp
                    type is (integer(kind=8))
                        ump = vp
                    type is (real(kind=4))
                        ump = vp
                    type is (real(kind=8))
                        ump = vp
                    class default
                        print *, 'Error! unmatched data types for um and v'
                        stop 1
                end select
            class default
                print *, 'Error! Unknown data type for um'
                stop 1
        end select
    End Subroutine



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

    function fum_add(um1, um2) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: um1, um2
        class(*),allocatable :: rst
        ! ··················· main body
        Select type(up1 => um1)
            type is (integer(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 + up2
                    type is (integer(kind=8))
                        rst = up1 + up2
                    type is (real(kind=4))
                        rst = up1 + up2
                    type is (real(kind=8))
                        rst = up1 + up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (integer(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 + up2
                    type is (integer(kind=8))
                        rst = up1 + up2
                    type is (real(kind=4))
                        rst = up1 + up2
                    type is (real(kind=8))
                        rst = up1 + up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 + up2
                    type is (integer(kind=8))
                        rst = up1 + up2
                    type is (real(kind=4))
                        rst = up1 + up2
                    type is (real(kind=8))
                        rst = up1 + up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 + up2
                    type is (integer(kind=8))
                        rst = up1 + up2
                    type is (real(kind=4))
                        rst = up1 + up2
                    type is (real(kind=8))
                        rst = up1 + up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            class default
                print *, 'Error! Unknown data type for um1'
                stop 1
        end select
    end function
    function fum_sub(um1, um2) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: um1, um2
        class(*),allocatable :: rst
        ! ··················· main body
        Select type(up1 => um1)
            type is (integer(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 - up2
                    type is (integer(kind=8))
                        rst = up1 - up2
                    type is (real(kind=4))
                        rst = up1 - up2
                    type is (real(kind=8))
                        rst = up1 - up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (integer(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 - up2
                    type is (integer(kind=8))
                        rst = up1 - up2
                    type is (real(kind=4))
                        rst = up1 - up2
                    type is (real(kind=8))
                        rst = up1 - up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 - up2
                    type is (integer(kind=8))
                        rst = up1 - up2
                    type is (real(kind=4))
                        rst = up1 - up2
                    type is (real(kind=8))
                        rst = up1 - up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 - up2
                    type is (integer(kind=8))
                        rst = up1 - up2
                    type is (real(kind=4))
                        rst = up1 - up2
                    type is (real(kind=8))
                        rst = up1 - up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            class default
                print *, 'Error! Unknown data type for um1'
                stop 1
        end select
    end function
    function fum_mul(um1, um2) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: um1, um2
        class(*),allocatable :: rst
        ! ··················· main body
        Select type(up1 => um1)
            type is (integer(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 * up2
                    type is (integer(kind=8))
                        rst = up1 * up2
                    type is (real(kind=4))
                        rst = up1 * up2
                    type is (real(kind=8))
                        rst = up1 * up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (integer(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 * up2
                    type is (integer(kind=8))
                        rst = up1 * up2
                    type is (real(kind=4))
                        rst = up1 * up2
                    type is (real(kind=8))
                        rst = up1 * up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 * up2
                    type is (integer(kind=8))
                        rst = up1 * up2
                    type is (real(kind=4))
                        rst = up1 * up2
                    type is (real(kind=8))
                        rst = up1 * up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 * up2
                    type is (integer(kind=8))
                        rst = up1 * up2
                    type is (real(kind=4))
                        rst = up1 * up2
                    type is (real(kind=8))
                        rst = up1 * up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            class default
                print *, 'Error! Unknown data type for um1'
                stop 1
        end select
    end function
    function fum_div(um1, um2) result(rst)
        implicit none
        ! ··················· Arguments
        class(*), intent(in) :: um1, um2
        class(*),allocatable :: rst
        ! ··················· main body
        Select type(up1 => um1)
            type is (integer(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 / up2
                    type is (integer(kind=8))
                        rst = up1 / up2
                    type is (real(kind=4))
                        rst = up1 / up2
                    type is (real(kind=8))
                        rst = up1 / up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (integer(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 / up2
                    type is (integer(kind=8))
                        rst = up1 / up2
                    type is (real(kind=4))
                        rst = up1 / up2
                    type is (real(kind=8))
                        rst = up1 / up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=4))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 / up2
                    type is (integer(kind=8))
                        rst = up1 / up2
                    type is (real(kind=4))
                        rst = up1 / up2
                    type is (real(kind=8))
                        rst = up1 / up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            type is (real(kind=8))
                select type(up2 => um2)
                    type is (integer(kind=4))
                        rst = up1 / up2
                    type is (integer(kind=8))
                        rst = up1 / up2
                    type is (real(kind=4))
                        rst = up1 / up2
                    type is (real(kind=8))
                        rst = up1 / up2
                    class default
                        print *, 'Error! unmatched data types for um1 and um2'
                        stop 1
                end select
            class default
                print *, 'Error! Unknown data type for um1'
                stop 1
        end select
    end function
End Module