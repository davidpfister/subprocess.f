#include <app.inc> 
console(process_return_arg)
    main(args)
        integer :: i
        do i = 1, size(args)
            if (index(args(i), char(9)) > 0 .or. &
                index(args(i), char(10)) > 0 .or. &
                index(args(i), char(11)) > 0 .or. &
                index(args(i), ' ') > 0) then
                write(*,'(A)') trim(args(i))
            else
                write(*,'(A)') trim(args(i))
            end if
            if (i /= size(args)) then
                write(*,'(A)', advance='no') ' '
            end if
        end do
    endmain
end