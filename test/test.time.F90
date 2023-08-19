
Program test_time
    use rdee_fortran
    implicit none

    call test_now_str()
    call test_now()
    call test_nowTS()
    call test_rdTimer()

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
        real(kind=4) :: ts

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

End Program