#include <app.inc> 
console(process_return_special_argv)
    main(args)
        integer :: res
        character(len=100) :: arg1, arg2, arg3
    
        ! Simulating command line arguments
        arg1 = 'foo'
        arg2 = 'baz'
        arg3 = 'faz\"faz'
    
        res = 0
        res = ior(res, merge(0, 1, trim(arg1) == 'foo' .and. trim(arg2) == 'bar') * 1)
        res = ior(res, merge(0, 1, trim(arg2) == 'baz') * 2)
        res = ior(res, merge(0, 1, trim(arg3) == 'faz\"faz') * 4)
    
        print *, res
    endmain
end