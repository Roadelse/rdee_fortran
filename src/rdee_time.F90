
Module rdee_time
    use rdee_ds
    implicit none
    
    type(dict) :: rdTimer_results


Contains

Function now_str() result(rst)
    implicit none
    ! ....................................... Return variable
    character(len=:), allocatable :: rst
    ! ....................................... Local variables
    character(8) :: dateS
    character(10) :: timeS
    ! ....................................... main body
    call date_and_time(dateS, timeS)

    rst = dateS(1:2)//'/'//dateS(3:4)//'/'//dateS(5:6)//' '//timeS(1:2)//':'//timeS(3:4)//':'//timeS(5:6)
    return
End Function now_str

subroutine now(year, month, day, hour, minute, second)
    ! from GPT-4 after several prompt @2023-08-18 14:39:38
    implicit none
    integer, intent(out), optional :: year, month, day, hour, minute, second
    integer :: values(8)

    call date_and_time(values=values)

    if (present(year)) then
        year = values(1)
    end if
    if (present(month)) then
        month = values(2)
    end if
    if (present(day)) then
        day = values(3)
    end if
    if (present(hour)) then
        hour = values(5)
    end if
    if (present(minute)) then
        minute = values(6)
    end if
    if (present(second)) then
        second = values(7)
    end if
end subroutine now

logical function isLeap(year)
    implicit none
    integer, intent(in) :: year
    isLeap = mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)
end function isLeap

integer function calTS(year, month, day, hour, minute, second)
    ! ***********************************************************
    ! This function aims to get time stamp
    ! with no consideration for leap seconds and milliseconds
    ! ***********************************************************
    implicit none
    ! ....................................... Argument
    integer, intent(in) :: year, month, day, hour, minute, second
    ! ....................................... local variables
    integer :: i, leap_years, normal_years, days_in_month, total_days
    ! ....................................... 

    days_in_month = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

    leap_years = (year - 1 - 1968) / 4 - (year - 1 - 1900) / 100 + (year - 1 - 1600) / 400
    normal_years = year - 1970 - leap_years

    if (isLeap(year)) then
        days_in_month(2) = 29
    end if

    total_days = normal_years * 365 + leap_years * 366
    do i = 1, month - 1
        total_days = total_days + days_in_month(i)
    end do
    total_days = total_days + day - 1

    nowTS = total_days * 24 * 3600 + hour * 3600 + minute * 60 + second
end function calTS

Function nowTS() result(rst)
    implicit none

    integer :: y, m, d, mi, hr, se

    call now(year=y, month=m, day=d, hour=hr, minute=mi, second=se)
    rst = calTS(y, m, d, hr, mi, se)

    return

End Function

Function rdTimer(id, init) result(rst)
    implicit none
    ! ....................................... Argument
    character(*), intent(in) :: id
    integer, intent(in), optional :: init
    ! ....................................... Local variable
    real(kind=4) :: rst
    ! ....................................... Local variable
    integer :: init_

    ! ....................................... main body
    ! >>>>>>>>>>>>>>>>>>> handle optinal arguments
    if (present(init)) then
        init_ = init
    else
        init_ = 0
    end if

    ! if (init_ .ne. 0) then



End Function

End Module
