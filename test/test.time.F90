
Program test_time
    use, intrinsic :: iso_c_binding
    ! use rdee_fortran
    ! use rdee_ds
    use rdee_time
    use rdee_string
    implicit none

    interface
        subroutine usleep_c(useconds) bind(c, name="usleep")
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: useconds
        end subroutine usleep_c
    end interface

    ! call test_now_str()
    ! call test_now()
    ! call test_nowTS()
    ! call test_rdTimer()

    ! call perf_nowTS()
    call test_rdProfiler

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
        call sleep(1)
        deltaTime = rdTimer('test')

        print *, 'delta time = ', deltaTime
        call assert(deltaTime .gt. 0.999d0, toString('Error in rdee_time, test_rdTimer, too quick, now is ', deltaTime))
        call assert(deltaTime .lt. 1.01d0, toString('Error in rdee_time, test_rdTimer, too slow, now is ', deltaTime))

        
        temp = rdTimer()
        call sleep(1)
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
        implicit none
        type(rdProfiler) :: rdp
        integer :: i, C = 0, N = 100000

        print *, '>>>>>>>>>>>>>>>>>>>>> now start to test rdProfiler'

        rdp = rdProfiler()
        do i = 1, N
            call rdp%start('k1')
            C = C*i / N + i
            call rdp%end('k1')
        end do

        C = -100
        ! call rdp%start('k3')
        do i = 1, 2
            call rdp%start('k2')
            ! C = C*i / N + i
            call usleep_c(500000)
            call rdp%end('k2')
        end do
        ! call rdp%end('k3')


        call rdp%print

    end Subroutine
End Program