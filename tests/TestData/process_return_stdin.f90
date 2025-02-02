#include <app.inc> 
console(process_return_stdin)
    main(args)
        character(len=20) :: str
        integer :: i
        character(len=1) :: input_char
    
        str = "abba are great!"
    
        do i = 1, len_trim(str) - 1
            read(*, '(A)') input_char
            if (str(i:i) /= input_char) then
                stop -1
            end if
        end do
    endmain
end