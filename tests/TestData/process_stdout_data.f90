#include <app.inc> 
console(process_stdout_data)
    main(args)
        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                                                 stderr => error_unit , &
                                                 stdin => input_unit
        integer :: i, e
        read(args(1)%chars, *) e
#if defined(__GFORTRAN__)
        call sleep(2)
#elif defined(__INTEL_COMPILER)
        block
            use ifport
            call sleep(2)
        end block
#endif

        do i = 0, e - 1
            write(stderr, '(Z1)') i - (i / 16) * 16
        end do
        stop 0
    endmain
end