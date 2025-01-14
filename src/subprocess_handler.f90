module subprocess_handler
    use, intrinsic :: iso_c_binding

    implicit none

    public  :: process_run, &
               process_finalize
    !public  :: process_close
    !public  :: process_readline
    !public  :: process_kill
    
    type, bind(c) :: iobuf
        type(c_ptr) :: PlaceHolder
    end type
    
    type, bind(c) :: subprocess_s
        type(iobuf) :: stdin_file
		type(iobuf) :: stdout_file
		type(iobuf) :: stderr_file
#if defined(_WIN32)
		type(c_ptr) :: hProcess
		type(c_ptr) :: hStdInput
		type(c_ptr) :: hEventOutput
		type(c_ptr) :: hEventError
#else
		integer(c_int) :: child
		integer(c_int) :: return_status
#endif
        integer(c_int) :: alive
    end type

    integer, parameter :: BUFFER_SIZE = 4096

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
    
        type(subprocess_s) function subprocess_stdin_c(process) bind(C, name='subprocess_stdin')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        type(subprocess_s) function subprocess_stdout_c(process) bind(C, name='subprocess_stdout')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        type(subprocess_s) function subprocess_stderr_c(process, out_return_code) bind(C, name='subprocess_stderr')
            import
            type(subprocess_s), intent(inout) :: process
            integer(c_int), intent(out) :: out_return_code
        end function
    
        integer(c_int) function subprocess_join_c(process) bind(C, name='subprocess_join')
            import
            type(subprocess_s), value :: process
        end function
    
        integer(c_int) function subprocess_destroy_c(process) bind(C, name='subprocess_destroy')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        integer(c_int) function subprocess_terminate_c(process) bind(C, name='subprocess_terminate')
            import
            type(subprocess_s), intent(inout) :: process
        end function
    
        integer(c_int) function subprocess_read_stdout_c(process, buffer, size) bind(C, name='subprocess_read_stdout')
            import
            type(subprocess_s), value :: process
            character(c_char), intent(inout) :: buffer(*)
            integer(c_int), intent(in), value :: size
        end function
    
        integer(c_int) function subprocess_read_stderr_c(process, buffer, size) bind(C, name='subprocess_read_stderr')
            import
            type(subprocess_s), value :: process
            character(c_char), intent(inout) :: buffer(*)
            integer(c_int), intent(in), value :: size
        end function
    
        type(subprocess_s) function fgets_c(str, numChars, stream) bind(C, name='fgets')
            import
            character(c_char), intent(inout) :: str(*)
            integer(c_int), intent(out) :: numChars
            type(subprocess_s), value :: stream
        end function

        function fputs_c(buf, handle) bind(C, name='fputs')
            import
            integer(c_int) :: fputs_c
            character(c_char), dimension(*) :: buf
            type(subprocess_s), value :: handle
        end function

        function fflush_c(handle) bind(C, name='fflush')
            import
            integer(c_int) :: fflush_c
            type(subprocess_s), value :: handle
        end function
    end interface

    interface process_run
        module procedure :: process_run_default
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
    integer function process_run_default(cmd, options, fp, ierr) result(pid)
        character(*), intent(in)                :: cmd
        integer, intent(in)                     :: options
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer :: istat

        istat = 0
        if (present(ierr)) ierr = istat

        pid = subprocess_create_c(cmd//c_null_char, options, fp%handle)
        if (pid < 0) then
            write (*, *) '*process_run* ERROR: Could not open pipe!'
            istat = -1
        end if
        istat = subprocess_join_c(fp%handle)
        istat = subprocess_destroy_c(fp%handle)
        if (present(ierr)) ierr = istat
    end function
    
    subroutine process_finalize(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr
        
        ierr = subprocess_destroy_c(fp%handle)
    end subroutine

end module
