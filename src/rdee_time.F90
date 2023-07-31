
Module rdee_time
    implicit none
    
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
    End Function


End Module