#include <app.inc> 
console(process_return_stdin)
    main(args)
		use iso_fortran_env, only: output_unit, input_unit
        character(len=15) :: str
        integer :: i
        character(200) :: input_char
    
        str = "abba_are_great!"
        do i = 1, len(str)
            read(input_unit, '(A)') input_char
			write(output_unit, '(A)', advance='no') trim(input_char)
            if (trim(input_char) == '!') then
                exit
            end if
        end do
    endmain
end