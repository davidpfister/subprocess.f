#include <app.inc> 
console(process_return_stdin_count)
    main(args)
        integer, parameter :: COUNT = 1
        character(200) :: temp
        logical :: eof_flag
        integer :: size
    
        size = 0
    
        do
            read(*, '(A)') temp
            size = size + len_trim(temp)
        
            if (len_trim(temp) > COUNT) then
                eof_flag = .true.
                exit
            end if
        end do
    
        if (eof_flag) then
            print *, size
        else
            print *, -1
        end if
    endmain
end