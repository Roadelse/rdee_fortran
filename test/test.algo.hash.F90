program main
    use rdee_algo
    implicit none

    print *, 'should be 3: ', hash_mod_is(3, 10)
    print *, 'test hello, should be 2: ', hash_mod_is('hello', 10)

end program