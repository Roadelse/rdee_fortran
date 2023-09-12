
Module rdee_time
    use rdee_ds
    implicit none
    
    type(dict) :: rdTimer_TS_dict
    real(kind=8) :: rdTimer_TS_array(10) = [0,0,0,0,0,0,0,0,0,0]

    Interface rdTimer
        module procedure rdTimer_by_id
        module procedure rdTimer_by_idx
    End Interface

    Type :: rdProfiler
        private
        type(dict) :: tcl  ! key : [total-time, count, last-time(reset to 0 after one pair of ticks)], do not forget recursive & elemental procedures?
      Contains
        Procedure :: start => rdProfiler_start
        Procedure :: end => rdProfiler_end
    End Type

    interface rdProfiler
        module procedure rdProfiler_constructor
    end interface

Contains

function rdProfiler_constructor() result(inst)
    implicit none
    type(rdProfiler) :: inst

    inst%tcl = dict()

end function

subroutine rdProfiler_start(this, id)
    implicit none
    class(rdProfiler), intent(in) :: this
    character(*), intent(in) :: id
    type(node) :: tcl_node

    if (this%tcl%hasKey(id)) then
        call this%tcl%get(id, tcl_node)
        if (tcl_node%item1d(3) .gt. 0) then
            return
        end if
        tcl_node%item1d(3) = nowTS()
    else
        call this%tcl%set(id, [0d0, 0d0, nowTS()])
    end if
end subroutine

subroutine rdProfiler_end(this, id)
    implicit none
    class(rdProfiler), intent(in) :: this
    character(*), intent(in) :: id
    type(node) :: tcl_node

    if (this%tcl%hasKey(id)) then
        call this%tcl%get(id, tcl_node)
        tcl_node%item1d(1) = tcl_node%item1d(1) + (nowTS() - tcl_node%item1d(3))
        tcl_node%item1d(2) = tcl_node%item1d(2) + 1
        tcl_node%item1d(3) = 0
    else
        print *, 'Error! missing rdProfiler%start operation'
        stop 1
    end if
end subroutine


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

subroutine now(year, month, day, hour, minute, second, millisecond)
    ! from GPT-4 after several prompt @2023-08-18 14:39:38
    implicit none
    integer, intent(out), optional :: year, month, day, hour, minute, second, millisecond
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
    if (present(millisecond)) then
        millisecond = values(8)
    end if
end subroutine now

logical function isLeap(year)
    implicit none
    integer, intent(in) :: year
    isLeap = mod(year, 4) == 0 .and. (mod(year, 100) /= 0 .or. mod(year, 400) == 0)
end function isLeap

real(kind=8) function calTS(year, month, day, hour, minute, second, millisecond)
    ! ***********************************************************
    ! This function aims to get time stamp
    ! with no consideration for leap seconds and milliseconds
    ! ***********************************************************
    implicit none
    ! ....................................... Argument
    integer, intent(in) :: year, month, day, hour, minute, second, millisecond
    ! ....................................... local variables
    integer :: i, leap_years, normal_years, total_days
    integer :: days_in_month(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

    ! ....................................... 

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

    calTS = total_days * 24 * 3600 + hour * 3600 + minute * 60 + second + millisecond / 1000d0
end function calTS

integer(kind=4) function calTSi(year, month, day, hour, minute, second)
    ! ***********************************************************
    ! This function aims to get time stamp
    ! with no consideration for leap seconds and milliseconds
    ! ***********************************************************
    implicit none
    ! ....................................... Argument
    integer, intent(in) :: year, month, day, hour, minute, second
    ! ....................................... local variables
    integer :: i, leap_years, normal_years, total_days
    integer :: days_in_month(12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)

    ! ....................................... 

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

    calTSi = total_days * 24 * 3600 + hour * 3600 + minute * 60 + second
end function calTSi

Function nowTS() result(rst)
    ! ***************************************************************
    ! This function aims to get the current time stamp 
    ! Please note that the result is based on UTC time, which is not correct
    ! Need to be updated later, maybe use C-bind functions to get UTC time
    ! ***************************************************************
    implicit none
    integer :: y, m, d, mi, hr, se, ms
    real(kind=8) :: rst

    call now(year=y, month=m, day=d, hour=hr, minute=mi, second=se, millisecond=ms)
    rst = calTS(y, m, d, hr, mi, se, ms)

    return

End Function

Integer(kind=4) Function nowTSi() result(rst)
    ! ***************************************************************
    ! This function aims to get the current time stamp 
    ! Please note that the result is based on UTC time, which is not correct
    ! Need to be updated later, maybe use C-bind functions to get UTC time
    ! ***************************************************************
    implicit none
    integer :: y, m, d, mi, hr, se

    call now(year=y, month=m, day=d, hour=hr, minute=mi, second=se)
    rst = calTSi(y, m, d, hr, mi, se)

    return

End Function nowTSi

Function rdTimer_by_id(id, init) result(rst)
    ! ***************************************************************
    ! This functions aims to provide a timer to calculate time interval
    ! between two invokes for this function
    ! use a rdee dict to load/store time-stamp values
    ! >>>>>>>>>>>>>>>>>>>>>>>
    ! Usage: 
    !       tmp = rdTimer('case1', init=1)  # init can be omitted
    !       tmp = rdTimer('case1')  # get results
    ! ***************************************************************
    implicit none
    ! ....................................... Argument
    character(*), intent(in) :: id
    integer, intent(in), optional :: init
    ! ....................................... Return variable
    real(kind=8) :: rst
    ! ....................................... Local variable
    integer :: init_
    real(kind=8) :: ts, ts0

    ! ....................................... main body
    ! >>>>>>>>>>>>>>>>>>> handle optinal arguments
    if (present(init)) then
        init_ = init
    else
        init_ = 0
    end if

    if (rdTimer_TS_dict%size .eq. 0) then
        rdTimer_TS_dict = dict()
    end if

    if (init_ .ne. 0 .or. .not. rdTimer_TS_dict%hasKey(id)) then
        call rdTimer_TS_dict%set(id, nowTS())
        rst = -1.
    else
        ts = nowTS()
        call rdTimer_TS_dict%get(id, ts0)
        rst = ts - ts0
    end if

    return
End Function


Function rdTimer_by_idx(idx, init) result(rst)
    ! ***************************************************************
    ! This functions aims to provide a more efficient way to ld/st stored 
    ! time-stamp via array
    ! Support max to 10 records
    ! >>>>>>>>>>>>>>>>>>>>>>>
    ! Usage: 
    !       tmp = rdTimer()  # space id equals to 1
    !       tmp = rdTimer()  # get results
    ! ***************************************************************
    implicit none
    ! ....................................... Argument
    integer, intent(in), optional :: idx
    integer, intent(in), optional :: init
    ! ....................................... Return variable
    real(kind=8) :: rst
    ! ....................................... Local variable
    integer :: init_, idx_

    ! ....................................... main body
    ! >>>>>>>>>>>>>>>>>>> handle optinal arguments
    if (present(init)) then
        init_ = init
    else
        init_ = 0
    end if
    if (present(idx)) then
        idx_ = idx
    else
        idx_ = 1
    end if

    if (idx_ .lt. 1 .or. idx_ .gt. 10) then
        print *, 'Error! rdee_time.rdTimer_TS_array is a 5-len array, index must be within 1...5'
        stop 1
    end if

    if (init_ .ne. 0 .or. rdTimer_TS_array(idx_) .eq. 0) then
        rdTimer_TS_array(idx_) = nowTS()
        rst = -1.
    else
        rst = nowTS() - rdTimer_TS_array(idx_)
    end if

    return
End Function

real(kind=8) function overhead_nowTS_1M() result(rst)
    implicit none
    real(kind=8) :: ts
    integer :: i
    integer, parameter :: N = 1000000
    ! ....................... main body
    rst = rdTimer()
    do i = 1, N
        ts = nowTS() + ts / 1e9
    end do
    rst = rdTimer()
    print *, '100000 times of nowTS() takes seconds: ',rst
end function overhead_nowTS_1M


End Module
