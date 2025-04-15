#include <app.inc> 
console(process_stderr_argv)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer :: i
        do i = 1, size(args)
            write(stderr, fmt='(dt, A)', advance='no') args(i), ' '
        end do
    endmain
end