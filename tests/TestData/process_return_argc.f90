#include <app.inc> 
console(process_return_arg)
    main(args)
        integer :: i, nargs
        
        
        nargs = size(args)
        print*, nargs
        
        stop 0
    endmain
end