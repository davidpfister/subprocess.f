#include <app.inc> 
console(process_combined_stdout_stderr)
    main(args)
		use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
												stderr => error_unit
		write(stdout, '(A)', advance='no') "Hello,"
		write(stderr, '(A)', advance='no') "It's me!"
		write(stdout, '(A)', advance='no') "world!"
		write(stderr, '(A)', advance='no') "Yay!"
    endmain
end