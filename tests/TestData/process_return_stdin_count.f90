#include <app.inc> 
console(process_return_stdin_count)
    main(args)
        integer, parameter :: COUNT = 16
        integer :: size, bytes
        character(len=COUNT) :: temp
        logical :: eof_flag
    
        size = 0
    
        do
            read(*, '(A)', iostat=bytes) temp
            if (bytes == 0) exit
            size = size + len_trim(temp)
        
            if (len_trim(temp) < COUNT) then
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