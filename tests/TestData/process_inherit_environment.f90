#include <app.inc> 
console(process_inherit_environment)
    main(args)
        character(:), allocatable :: str
        integer :: status, return_value

        status = get_env('PROCESS_ENV_TEST', str)
        if (len_trim(str) > 0) then
            return_value = atoi(trim(str))
        else
            return_value = 0
        end if
        
        print *, return_value
    endmain

    function get_env(var_name, value) result(status)
        character(*), intent(in)    :: var_name
        character(:), allocatable, intent(out) :: value
        integer :: status
        
        allocate(character(255) :: value)
        call get_environment_variable(var_name, value)

        value = trim(value)
        if (len_trim(value) > 0) then
            status = 0
        else
            status = 1
        end if
    end function
      
    function atoi(str) result(num)
        character(*), intent(in)    :: str
        integer :: num
        
        read(str, *) num
    end function
end