#include <app.inc> 
console(process_stdout_argc)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        write(stdout, '(I0)') size(args)
    endmain
end