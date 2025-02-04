#include <app.inc> 
console(process_hung)
    main(args)
        integer :: r
        r = 0
        do while (return_0_non_optimizable() == 0)
            r = r + 1
        end do
    
        print *, "r=", r
    endmain

    integer function return_0_non_optimizable() result(res)
        character(len=100) :: buffer
        integer :: value
        character(len=1) :: c
        integer :: result, digit, i
        
        value = 62831853
        write(buffer, '(I10)') value
        result = 0
        
        do i = 1, len_trim(buffer)
            c = buffer(i:i)
            digit = iachar(c) - iachar('0')
            result = result + digit
        end do
        
        res = result - 36
    end function
end