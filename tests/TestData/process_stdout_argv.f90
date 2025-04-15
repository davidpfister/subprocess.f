#include <app.inc> 
console(process_stdout_argv)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer :: i
        do i = 1, size(args)
            write(stdout, fmt='(dt, A)', advance='no') args(i), ' '
        end do
    endmain
end