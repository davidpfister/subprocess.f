#include <stdio.inc>
#include <app.inc> 
console(process_stdout_poll)

    main(args)
        use unix_stdio
        use, intrinsic :: iso_c_binding
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer         :: i, max
        integer         :: c
        integer         :: fd     ! File descriptor.
        type(c_ptr)     :: stream ! Input stream.

        max = 0
        
        if (size(args) > 0) read(args(1)%chars, *) max

        stream = c_fdopen(fnum(stdin), 'r' // c_null_char)

        do
            do i = 1, max
                write(stdout, *) "Hello, world!"
                flush(stdout)
            end do
            c = c_fgetc(stream)
            if (achar(c) == 's') exit
        end do
        stop 0
    endmain

    integer function fnum(num) result(res)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer, intent(in) :: num

        select case(num)
        case (stdin)
            res = 0
        case (stdout)
            res = 1
        case (stderr)
            res = 2
        case default
            res = num
        end select
    end function
end