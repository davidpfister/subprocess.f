#include <app.inc> 
console(process_combined_stdout_stderr)
    main(args)
		use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
												stderr => error_unit
		type(string), intent(in) :: args(:)

		write(stdout, "Hello,")
		flush(stdout);
		write(stdout, "It's me!")
		flush(stdout);
		write(stdout, "world!")
		flush(stdout);
		write(stderr, "Yay!\n")
		flush(stderr);
	stop 0
    endmain
end