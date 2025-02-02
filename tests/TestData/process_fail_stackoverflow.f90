#include <app.inc> 
console(process_return_arg)
    main(args)
        integer :: x, y
    
        x = 5
        y = fun(x)
        print *, y
    endmain
  
    integer function return_0_non_optimizable()
        character(len=100) :: buffer
        integer :: value, digit, result
        character(len=1) :: c
    
        value = 62831853
        write(buffer, '(I0)') value
        result = 0
        c = buffer(1:1)
        
        do while (c /= ' ')
        digit = ichar(c) - ichar('0')
        result = result + digit
        c = buffer(ichar(c) + 1:ichar(c) + 1)
        end do
        
        return_0_non_optimizable = result - 36
    end function
  
    integer function fun(x)
        integer :: x
    
        if (x == 1) then
        fun = 5
        return
        end if
        
        x = 6
        if (return_0_non_optimizable() == 0) then
        fun = fun(x)
        return
        end if
        
        fun = x
    end function
end