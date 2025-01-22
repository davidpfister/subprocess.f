#include <app.inc> 
console(process_return_arg)
    subroutine main(args)
        type(string), intent(in) :: args(:)
        integer :: i, nargs
        
        
        nargs = size(args)
        print*, nargs
        
        stop 0
    end subroutine
end