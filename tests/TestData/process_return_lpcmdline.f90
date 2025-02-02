#include <app.inc> 
console(process_return_arg)
    main(args)
        integer :: i
        do i = 1, size(args)
            if (index(args(i)%chars, char(9)) > 0 .or. &
                index(args(i)%chars, char(10)) > 0 .or. &
                index(args(i)%chars, char(11)) > 0 .or. &
                index(args(i)%chars, ' ') > 0) then
                write(*,'(A') trim(args(i)%chars)
            else
                write(*,'(A)') trim(args(i)%chars)
            end if
            if (i /= size(args)) then
                write(*,'(A)', advance="no") " "
            end if
        end do
    endmain
end