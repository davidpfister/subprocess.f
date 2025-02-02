#include <app.inc> 
console(process_stdout_large)

    main(args)
        use iso_c_binding
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit
        integer :: i, max
        max = 0
        
        if (size(args) > 0) then
            read(args(1)%chars, *) max
        end if

        do i = 1, max
            write(stdout, *) "Hello, world!"
        end do
        stop 0
    endmain
end