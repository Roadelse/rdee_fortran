
Program test_time
    use, intrinsic :: iso_c_binding
    ! use rdee_fortran
    ! use rdee_ds
    use rdee_time
    use rdee_string
    implicit none
    character(64) :: arg1

    interface
        subroutine usleep_c(useconds) bind(c, name="usleep")
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: useconds
        end subroutine usleep_c
        subroutine sleep_c(seconds) bind(C, name="sleep")
            use iso_c_binding, only: c_int
            implicit none
            integer(kind=c_int), value :: seconds
        end subroutine sleep_c
    end interface


    call get_command_argument(1, arg1)

    if (arg1 .eq. 'ALL' .or. arg1 .eq. 'all' .or. arg1 .eq. '') then
        call test_now_str()
        call test_now
        call test_nowTS
        call test_rdTimer
        call perf_nowTS
        call test_rdProfiler
        call test_rdDateTime
    elseif (arg1 .eq. 'test_rdProfiler') then
        call test_rdProfiler
    elseif (arg1 .eq. 'perf_nowTS') then
        call perf_nowTS
    elseif (arg1 .eq. 'test_rdTimer') then
        call test_rdTimer
    elseif (arg1 .eq. 'test_nowTS') then
        call test_nowTS
    elseif (arg1 .eq. 'test_now') then
        call test_now
    elseif (arg1 .eq. 'test_now_str') then
        call test_now_str
    elseif (arg1 .eq. 'test_rdDateTime') then
        call test_rdDateTime
    else
        print *, 'unknwon argument!'
        stop 1
    end if 


contains

subroutine test_now_str()
    implicit none

    print *, now_str()

end subroutine

subroutine test_now()
    implicit none

    integer :: y, m, d, mi, hr, se

    call now(year=y, month=m, day=d, hour=hr, minute=mi, second=se)

    print *, y, m, d
    call assert(y .ge. 2023, 'Error, illegal date & time values')
    call assert(m .gt. 0, 'Error, illegal date & time values')
    call assert(d .gt. 0 .and. d .lt. 32, 'Error, illegal date & time values')
    call assert(mi .ge. 0 .and. mi .le. 60, 'Error, illegal date & time values')
    call assert(hr .ge. 0 .and. hr .le. 60, 'Error, illegal date & time values')
    call assert(se .ge. 0 .and. se .le. 60, 'Error, illegal date & time values')

    print *, 'correct for now(*), possibly.'


end subroutine

subroutine test_nowTS()
    implicit none
    real(kind=8) :: ts

    ts = nowTS()
    print *, 'current time stamp is ', ts

end subroutine

subroutine test_rdTimer()
    implicit none
    real(kind=8) :: deltaTime, temp

    print *, '>>>>>>>>>>>>>>>>>>>>> now start to test rdTimer'

    temp = rdTimer('test', 1)
    call sleep_c(1)
    deltaTime = rdTimer('test')

    print *, 'delta time = ', deltaTime
    call assert(deltaTime .gt. 0.999d0, toString('Error in rdee_time, test_rdTimer, too quick, now is ', deltaTime))
    call assert(deltaTime .lt. 1.01d0, toString('Error in rdee_time, test_rdTimer, too slow, now is ', deltaTime))

    
    temp = rdTimer()
    call sleep_c(1)
    deltaTime = rdTimer()
    print *, 'delta time = ', deltaTime
    call assert(deltaTime .gt. 0.999d0, toString('Error in rdee_time, test_rdTimer, too quick, now is ', deltaTime))
    call assert(deltaTime .lt. 1.01d0, toString('Error in rdee_time, test_rdTimer, too slow, now is ', deltaTime))

end Subroutine

subroutine perf_nowTS()
    implicit none

    real(kind=8) :: t1, ts
    integer :: i, N = 2000000

    print *, '>>>>>>>>>>>>>>>>>>>>> Start to calculate time cost for nowTS, repeating 10000 times'

    t1 = rdTimer()
    do i = 1, N
        ts = nowTS()!  + ts / 1e9
    end do
    t1 = rdTimer()
    print *, 'delta time = ', t1, ' for ', N, ' times nowTS() executions'
    print *, toString(t1 / 2000000 * 1e9, ' ns per times')
    
end subroutine


subroutine test_rdProfiler()
#ifdef MPI
    use mpi
#endif
    implicit none
    type(rdProfiler) :: rdp
    integer :: i, C = 0, N = 100000
    integer :: istat

    print *, ''
    print *, '>>>>>>>>>>>>>>>>>>>>> now start to test rdProfiler'
#ifdef MPI
    call MPI_INIT(istat)
#endif
    rdp = rdProfiler(use_mpi=.true.)
    call rdp%start('k3')
    do i = 1, N
        call rdp%start('k1')
        C = C*i / N + i
        ! call usleep_c(5000)
        call rdp%end('k1')
    end do
    call rdp%end('k3')

    C = -100
    do i = 1, 2
        call rdp%start('k2')
        ! C = C*i / N + i
        call usleep_c(500000)
        call rdp%end('k2')
    end do

    call rdp%print
    call rdp%print(out='test_rdProfiler.sct.csv')


    ! ........ test rdp0
    rdp0 = rdProfiler()
    call rdp0%start('q')
    call rdp0%end('q')
    print *, '>>>>>>>>>>>>>>>>>>>>> pass test_rdProfiler (no manual check actually)'
    print *, ''

#ifdef MPI
    call MPI_FINALIZE(istat)
#endif

end Subroutine test_rdProfiler

Subroutine test_rdDateTime()
    implicit none

    type(rdDateTime) :: dtm1

    print *, '>>>>>>>>>>> Enter test_rdDateTime <<<<<<<<<<<<<'
    dtm1 = rdDateTime(y=2018, m=1, d=1)
    print *, 'cp1'
    print *, dtm1%toString()
    print *, 'cp2'
    call assert(dtm1%toString() .eq. '20180101', 'Error in rdDateTime%toString')
    print *, dtm1%toString('%Y/%m/%d %H')
    call assert(dtm1%toString('%Y/%m/%d %H') .eq. '2018/01/01 00', 'Error in rdDateTime%toString, with format')

    call dtm1%fromString('20231007 1513')
    call dtm1%print
    call assert(dtm1%toString('%y/%m/%d %H') .eq. '23/10/07 15', 'Error in rdDateTime%fromString, from rdee_time')
    call dtm1%fromString('20231007 1513', format='%Y%m%d %H%M')
    call assert(dtm1%toString('%y/%m/%d %H') .eq. '23/10/07 15', 'Error in rdDateTime%fromString, from rdee_time')

End Subroutine

End Program