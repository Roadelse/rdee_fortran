
Module rdee_time
    use rdee_ds
    use rdee_string
    implicit none
    
    type(dict) :: rdTimer_TS_dict
    real(kind=8) :: rdTimer_TS_array(10) = [0,0,0,0,0,0,0,0,0,0]

    Type, Public :: rdDateTime
        Integer(kind=4) :: year=-1, month=-1, day=-1, hour=-1, minute=-1, second=-1, msecond=0, usecond=0

      Contains
        Procedure :: fromString => rddt_fromString
        Procedure :: toString => rddt_toString
        Procedure :: checkValid => rddt_checkValid
        Procedure :: print => rddt_print

    End Type
    Interface rdDateTime
        Module Procedure rdDateTime_constructor
    End Interface

    Interface rdTimer
        module procedure rdTimer_by_id
        module procedure rdTimer_by_idx
    End Interface

    Type :: rdProfiler
        ! ***************************************************
        ! Do not support: 
        !     1. self recursive : a->a->a->a->...
        !     2. circular recursive : a->b->a->b->...
        ! current weakness : 
        !     overhead : ~5s for 1M pairs
        ! ***************************************************
        private
        type(dict) :: tcl   ! key : [total-time, count, last-time]
                            ! last-time is reset to 0 after each rdProfiler_end
                            ! do not forget recursive & elemental procedures?? (seems ok now)
        type(dict) :: keyInc
        type(list) :: keyStack
        character(128) :: sechain   ! section chain,  e.g., cmaq.aero.aeroproc
        integer(kind=4) :: current_section_id_length
        logical :: use_mpi = .false.
        integer(kind=4) :: pid = 0
        Contains
        Procedure :: start => rdProfiler_start
        Procedure :: end => rdProfiler_end
        Procedure :: print => rdProfiler_print
    End Type

    Type(rdProfiler) :: rdp0

    interface rdProfiler
        module procedure rdProfiler_constructor
    end interface

    Interface assignment(=)
    
    End Interface

Contains

Function rdDateTime_constructor(y, m, d, hh, mm, ss) result(rst)
    implicit none

    integer(kind=4), intent(in), optional :: y, m, d, hh, mm, ss
    type(rdDateTime) :: rst

    if (present(y)) rst%year = y
    if (present(m)) rst%month = m
    if (present(d)) rst%day = d
    if (present(hh)) rst%hour = hh
    if (present(mm)) rst%minute = mm
    if (present(ss)) rst%second = ss
    call rst%checkValid
End Function

Subroutine rddt_checkValid(this)
    implicit none
    ! ................................. Argument
    class(rdDateTime), intent(inout) :: this
    ! ................................. Main Body
    call assert(this%year .gt. 1700 .and. this%year .lt. 2500 .or. this%year .eq. -1, 'Error! Invalid year in rdDateTime')
    call assert(this%month .ge. 1 .and. this%month .le. 12 .or. this%month .eq. -1, 'Error! Invalid month in rdDateTime')
    call assert(this%day .ge. 1 .and. this%day .le. 31 .or. this%day .eq. -1, 'Error! Invalid day in rdDateTime')
    call assert(this%hour .ge. 0 .and. this%hour .le. 23 .or. this%hour .eq. -1, toString('Error! Invalid hour in rdDateTime, now is:', this%hour))
    call assert(this%minute .ge. 0 .and. this%minute .le. 59 .or. this%minute .eq. -1, 'Error! Invalid minute in rdDateTime')
    call assert(this%second .ge. 0 .and. this%second .le. 59 .or. this%second .eq. -1, 'Error! Invalid second in rdDateTime')
End Subroutine

Subroutine rddt_print(this)
    implicit none
    ! ................................. Argument
    class(rdDateTime), intent(inout) :: this
    ! ................................. Main Body
    print *, 'year=',this%year
    print *, 'month=',this%month
    print *, 'day=',this%day
    print *, 'hour=',this%hour
    print *, 'minute=',this%minute
    print *, 'second=',this%second
End Subroutine

Subroutine rddt_fromString(this, dtmStr, format)
    implicit none
    ! ................................. Argument
    class(rdDateTime), intent(inout) :: this
    character(len=*), intent(in) :: dtmStr  ! datetime string
    character(len=*), intent(in), optional :: format
    ! ................................. Local variables
    character(len=30) :: format_
    integer(kind=4) :: i, j, k, L
    character(len=:), allocatable :: Sdigits

    ! ................................. Main Body
    ! >>>>>>>>>>>>>>>>>> handle optional arguments
    if (present(format)) then
        call assert(len_trim(format) .lt. 30, 'Error in rdDatetime%fromString! Unknown string format.')
        ! >>>>>>>>>>>>>>>>>> resolve format
        i = 1
        j = 1
        do while(i .le. len_trim(format))
            if (format(i:i) .eq. '%') then
                i = i + 1
                Select case (format(i:i))

                case ('Y')  ! year: 1999, 2023, ...
                    this%year = s2i4(dtmStr(j:j+3))
                    j = j + 4
                case ('y')  ! year: 99, 00, 03, 13, 23, ...
                    k = s2i4(dtmStr(j:j+1))
                    if (k .ge. 69) then
                        this%year = 1900 + k
                    else
                        this%year = 2000 + k
                    end if
                    j = j + 2
                case ('m')  ! month: 01, 02, 03, ... 
                    this%month = s2i4(dtmStr(j:j+1))
                    j = j + 2
                case ('d')  ! day: 01, 02, ..., 31
                    this%day = s2i4(dtmStr(j:j+1))
                    j = j + 2
                case ('H')  ! hour : 00, 01, ..., 23
                    this%hour = s2i4(dtmStr(j:j+1))
                    j = j + 2
                case ('M')  ! Minute : 00, 01, ..., 59
                    this%minute = s2i4(dtmStr(j:j+1))
                    j = j + 2
                case ('S')  ! Second : 00, 01, ..., 59
                    this%second = s2i4(dtmStr(j:j+1))
                    j = j + 2
                case default
                    print *, 'Error in rdDatetime%fromString! Unknown flag: '//trim(format_(i-1:i))
                    stop 1
                end select
                i = i + 1
            else
                i = i + 1
            end if
        end do
    else
        i = 1
        j = 1
        Sdigits = S3digits(dtmStr)
        print *, 'Sdigits=',Sdigits
        L = len(Sdigits)
        if (L .ge. 4) this%year = s2i4(Sdigits(1:4))
        if (L .ge. 6) this%month = s2i4(Sdigits(5:6))
        if (L .ge. 8) this%day = s2i4(Sdigits(7:8))
        if (L .ge. 10) this%hour = s2i4(Sdigits(9:10))
        if (L .ge. 12) this%minute = s2i4(Sdigits(11:12))
        if (L .ge. 14) this%second = s2i4(Sdigits(13:14))
        deallocate(Sdigits)
    end if
    call this%checkValid
End Subroutine rddt_fromString


Function rddt_toString(this, format1) result(rst)
    implicit none
    ! ................................. Arguments
    class(rdDateTime), intent(inout) :: this
    character(len=*), intent(in), optional :: format1
    ! ................................. Local variables
    character(len=30) :: format_
    integer(kind=4) :: i, j, k
    Character(len=80) :: rstHolder
    ! ................................. Return variable
    Character(len=:), allocatable :: rst

    ! ................................. Main Body
    print *, 'Enter toString'
    rstHolder = ''
    ! >>>>>>>>>>>>>>>>>> handle optional arguments
    if (present(format1)) then
        call assert(len_trim(format1) .lt. 30, 'Error in rdDatetime%fromString! Unknown string format.')
        format_ = format1
    else
        format_ = '%Y%m%d'
        if (this%hour + this%minute + this%second .gt. 0) then
            format_ = '%Y%m%d%H%M%S'
        end if
    end if
    print *, 'enter toString, format=', format_
    ! >>>>>>>>>>>>>>>>>> resolve format
    i = 1
    j = 1
    do while(i .le. len_trim(format_))
        if (format_(i:i) .eq. '%') then
            i = i + 1
            Select case (format_(i:i))

            case ('Y')  ! year: 1999, 2023, ...
                ! this%year = s2i4(format_(i:i+3))
                rstHolder(j:j+3) = i42s(this%year)
                j = j + 4
            case ('y')  ! year: 99, 00, 03, 13, 23, ...
                write(rstHolder(j:j+1), '(I0.2)') mod(this%year, 100)
                j = j + 2
            case ('m')  ! month: 01, 02, 03, ... 
                write(rstHolder(j:j+1), '(I0.2)') this%month
                j = j + 2
            case ('d')  ! day: 01, 02, ..., 31
                write(rstHolder(j:j+1), '(I0.2)') this%day
                j = j + 2
            case ('H')  ! hour : 00, 01, ..., 23
                call assert(this%hour .ge. 0, 'Error! cannot specify %H for a rddt without hour definition!')
                write(rstHolder(j:j+1), '(I0.2)') this%hour
                j = j + 2
            case ('M')  ! Minute : 00, 01, ..., 59
                call assert(this%minute .ge. 0, 'Error! cannot specify %M for a rddt without minute definition!')
                write(rstHolder(j:j+1), '(I0.2)') this%minute
                j = j + 2
            case ('S')  ! Second : 00, 01, ..., 59
                call assert(this%second .ge. 0, 'Error! cannot specify %S for a rddt without second definition!')
                write(rstHolder(j:j+1), '(I0.2)') this%second
                j = j + 2
            case default
                print *, 'Error in rdDatetime%fromString! Unknown flag: '//trim(format_(i-1:i))
                stop 1
            end select
            i = i + 1
        else
            rstHolder(j:j) = format_(i:i)
            i = i + 1
            j = j + 1
        end if
    end do
    ! print *, 'len=',len_trim(rstHolder)
    rst = trim(rstHolder)
End Function rddt_toString

function rdProfiler_constructor(use_mpi) result(inst)
#ifdef MPI
    use mpi
#endif
    implicit none
    type(rdProfiler) :: inst
    logical, intent(in), optional :: use_mpi
    integer(kind=4) :: istat

    inst%tcl = dict()
    inst%keyInc = dict()
    ! inst%visited = dict() 
#ifdef MPI
    if (present(use_mpi)) then
        inst%use_mpi = use_mpi
        if (use_mpi) then
            call mpi_comm_rank(MPI_COMM_WORLD, inst%pid, istat)
        end if
    end if
#endif

end function

subroutine rdProfiler_start(this, id)
    implicit none
    class(rdProfiler), intent(inout) :: this
    character(*), intent(in) :: id
    type(node),pointer :: tcl_node
    ! character(len=:),allocatable :: idc  ! id chain

    if (this%keyStack%has(id)) then
        print *, 'Error! do not support recursive by now, neither self recursive nor circular recursive'
        stop 1
    end if

    call this%keyStack%append(id)
    this%sechain = this%keyStack%join('->')
    ! print *, 'sechain = ',this%sechain

    if (this%tcl%hasKey(this%sechain)) then
        tcl_node => this%tcl%fp2node(this%sechain)
        call um_assign(tcl_node%item1d(3), nowTS())
    else
        call this%tcl%set(trim(this%sechain), [0d0, 0d0, nowTS()])
    end if
end subroutine

subroutine rdProfiler_end(this, id)
    implicit none
    class(rdProfiler), intent(inout) :: this
    character(*), intent(in) :: id
    type(node),pointer :: tcl_node
    
    this%sechain = this%keyStack%join('->')
    tcl_node => this%tcl%fp2node(trim(this%sechain))
    ! call this%tcl%set(id, [um2i8(tcl_node%item1d(1)) + nowTS() - um2i8(tcl_node%item1d(3)), um2i8(tcl_node%item1d(2)) + 1, 0d0])
    ! tcl_node%item1d(1) = tcl_node%item1d(1) + (nowTS() - tcl_node%item1d(3))
    ! tcl_node%item1d(2) = tcl_node%item1d(2) + 1
    ! tcl_node%item1d(3) = 0
    select type(v1d => tcl_node%item1d)
        type is (real(kind=8))
            v1d(1) = v1d(1) + (nowTS() - v1d(3))
            v1d(2) = v1d(2) + 1
            v1d(3) = 0d0
    end select
    ! call tcl_node%print

    call this%keyStack%popLast()
end subroutine

Subroutine rdProfiler_print(this, out)
    implicit none
    class(rdProfiler), intent(inout) :: this
    character(*), intent(in), optional :: out
    type(list) :: keys
    type(node), pointer :: np, np2

    integer :: istat, logunit

    if (present(out)) then
        if (out .eq. "no" .or. out .eq. "none" .or. out .eq. "N" .or. out .eq. "F" .or. out .eq. "skip") then
            logunit = -1
        else
            if (this%use_mpi) then
                open(unit=101, file=toString('p', this%pid, '.', out), status='unknown', action='write', iostat=istat)
            else
                open(unit=101, file=out, status='unknown', action='write', iostat=istat)
            end if
            if (istat .ne. 0) then
                print *, 'Error! Failed to open file: '//trim(out)
                stop 1
            end if
            logunit = 101
        end if
    else
        logunit = 6
    end if

    if (logunit .gt. 0) then
        keys = this%tcl%keys()
        np => keys%head

        write(logunit, '(A)') 'section,count,time'
        do while(associated(np))
            np2 => this%tcl%fp2node(np%item)
            write(logunit, '(A,",",I0,",",1PG0)') um2s(np%item), um2i4(np2%item1d(2)), um2r8(np2%item1d(1))
            np => np%next
        end do
        if (logunit .ne. 6) close(logunit)    
    end if

End Subroutine


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
