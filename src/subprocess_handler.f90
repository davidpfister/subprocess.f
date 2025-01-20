module subprocess_handler
    use, intrinsic :: iso_c_binding

    implicit none

    public  :: internal_run, &
               internal_finalize, &
               internal_wait, &
               internal_read_stdout
    !public  :: process_close
    !public  :: process_readline
    !public  :: process_kill
    
    type, bind(c) :: subprocess_s
        type(c_ptr) :: stdin_file   !FILE*
		type(c_ptr) :: stdout_file  !FILE*
		type(c_ptr) :: stderr_file  !FILE*
#if defined(_WIN32)
		type(c_ptr) :: hProcess     !void*
		type(c_ptr) :: hStdInput    !void*
		type(c_ptr) :: hEventOutput !void*
		type(c_ptr) :: hEventError  !void*
#else
		integer(c_int) :: child
		integer(c_int) :: return_status
#endif
        integer(c_int) :: alive
    end type

    integer, parameter :: BUFFER_SIZE = 4095

    type, public :: handle_pointer
        private
        type(subprocess_s) :: handle
    end type

    interface
        integer(c_int) function subprocess_create_c(command_line, options, out_process) bind(C, name='subprocess_create')
            import
            character(c_char), dimension(*), intent(in) :: command_line
            integer(c_int), intent(in), value :: options
            type(subprocess_s), intent(inout) :: out_process
        end function
        
        integer(c_int) function subprocess_create_ex_c(command_line, options, environment, out_process) bind(C, name='subprocess_create_ex')
            import
            character(c_char), dimension(*) :: command_line
            integer(c_int), intent(in), value :: options
            character(c_char), dimension(*) :: environment
            type(subprocess_s), intent(inout) :: out_process
        end function
    
        type(c_ptr) function subprocess_stdin_c(process) bind(C, name='subprocess_stdin')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        type(c_ptr) function subprocess_stdout_c(process) bind(C, name='subprocess_stdout')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        type(c_ptr) function subprocess_stderr_c(process) bind(C, name='subprocess_stderr')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        integer(c_int) function subprocess_join_c(process, out_return_code) bind(C, name='subprocess_join')
            import
            type(subprocess_s), intent(inout) :: process
            integer(c_int), intent(out) :: out_return_code
        end function
    
        integer(c_int) function subprocess_destroy_c(process) bind(C, name='subprocess_destroy')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        integer(c_int) function subprocess_terminate_c(process) bind(C, name='subprocess_terminate')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        integer(c_long) function subprocess_read_stdout_c(process, buffer, size) bind(C, name='subprocess_read_stdout')
            import
            type(subprocess_s), intent(inout) :: process
            character(c_char), intent(inout) :: buffer(*)
            integer(c_long), value :: size
        end function
    
        integer(c_int) function subprocess_read_stderr_c(process, buffer, size) bind(C, name='subprocess_read_stderr')
            import
            type(subprocess_s), intent(inout) :: process
            character(c_char), intent(inout) :: buffer(*)
            integer(c_int), intent(out) :: size
        end function
    
        integer(c_int) function subprocess_alive_c(process) bind(C, name='subprocess_alive')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        type(c_ptr) function fgets_c(str, numChars, stream) bind(C, name='fgets')
            import
            character(c_char), intent(inout) :: str(*)
            integer(c_int), intent(inout) :: numChars
            type(c_ptr), value :: stream
        end function

        integer(c_int) function fputs_c(buf, handle) bind(C, name='fputs')
            import
            character(c_char), intent(inout) :: buf(*)
            type(c_ptr), value :: handle
        end function

        function fflush_c(handle) bind(C, name='fflush')
            import
            integer(c_int) :: fflush_c
            type(c_ptr), value :: handle
        end function
    end interface

    interface internal_run
        module procedure :: internal_run_default
    end interface
        
contains

    !> @brief Execute a child program, determine whether it succeeded or failed based
    !! on the last line of text that the program wrote to its standard output.
    !!
    !! @param[in]     cmd     The command line to execute.  Typically
    !! the first argument would be the same as @a name.
    !!
    !! @param[out]    success_flag      Flag to indicate whether successful
    !! termination of the cmd was detected.  This is based on the last
    !! line of text that was written by the program - if it is the same as
    !! success_text then it is assumed that the program succeeded.
    integer function internal_run_default(cmd, options, fp, ierr) result(pid)
        character(*), intent(in)                :: cmd
        integer, intent(in)                     :: options
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer :: istat
        integer(c_int) :: rcode
        
        istat = 0
        
        pid = subprocess_create_c(cmd//c_null_char, options, fp%handle)
        if (pid < 0) then
            write (*, *) '*process_run* ERROR: Could not create process!'
            istat = -1
        end if
        
        istat = subprocess_join_c(fp%handle, rcode)
        
        if (present(ierr)) ierr = istat
    end function
    
    function internal_read_stdout(fp) result(output)
        type(handle_pointer), intent(inout)     :: fp
        character(:), allocatable :: output
        !private
        integer(c_long) :: l
        character(BUFFER_SIZE) :: buf

        l = -1
        buf = ' '
        allocate(character(0)::output)
        l = subprocess_read_stdout_c(fp%handle, buf, BUFFER_SIZE)
        if (l > 0) then 
            output = trim(buf(:l))
        end if
    end function
    
    function internal_isalive(fp, ierr) result(alive)
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        logical :: alive
        !private
        integer :: status
        
        status = subprocess_alive_c(fp%handle)
        alive = (status /= 0)
    end function
    
    subroutine internal_wait(fp, ierr)
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer :: istat
        integer(c_int) :: rcode

        istat = 0
        istat = subprocess_join_c(fp%handle, rcode)
        if (present(ierr)) ierr = istat
    end subroutine
    
    subroutine internal_finalize(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr
        
        ierr = subprocess_destroy_c(fp%handle)
    end subroutine

end module
