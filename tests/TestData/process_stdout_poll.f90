#include <app.inc> 
console(process_stdout_poll)

    main(args)
        use, intrinsic :: iso_c_binding
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer         :: i, max
        character(len=100) :: input

        max = 0
        
        if (size(args) > 0) read(args(1)%chars, *) max

        do
            do i = 1, max
                write(stdout, *) "Hello, world!"
                flush(stdout)
            end do
            
            read(stdin, *) input
            if (trim(input) == 's') exit
        end do
        stop 0
    endmain
end