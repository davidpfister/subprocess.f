#include <app.inc> 
console(process_inherit_environment)
    main(args)
        character(len=100) :: str
        integer :: status, return_value

        if (size(args) > 1 .and. trim(args(2)%char) == "all") then
            if (get_environment_variable("PROCESS_ENV_TEST", str) == 0) then
                return_value = 0
            else
                return_value = 1
            end if
        else
            status = get_environment_variable("PROCESS_ENV_TEST", str)
            if (len_trim(str) > 0) then
                return_value = atoi(trim(str))
            else
                return_value = 0
            end if
        end if
        
        print *, return_value
    endmain

    function get_environment_variable(var_name, value) result(status)
        character(*), intent(in)    :: var_name
        character(100), intent(out) :: value
        integer :: status
        
        value = getenv(var_name)
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