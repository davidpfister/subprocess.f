#include <app.inc> 
console(process_stdout_large)

    main(args)
        use iso_c_binding
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit, &
                                                 stdin => input_unit
        integer :: i, max

        integer(c_int) function fgetc_c(desc) bind(C, name='fgetc')
            type(c_ptr), intent(in), value :: desc
        end function

        type(c_ptr) function fdopen_c(num_desc, mode) bind(C, name='fdopen') result(desc)
            integer(c_int), value :: num_desc
            character(c_char), intent(in) :: mode(*)
        end function

        max = 0
        
        if (size(args) > 0) then
            read(args(1)%chars, *) max
        end if

        do while (fgetc_c(fdopen_c(stdin, 'r' // c_null_char)))
            do i = 1, max
                write(stdout, *) "Hello, world!"
            end do
        end do
        stop 0
    endmain
end