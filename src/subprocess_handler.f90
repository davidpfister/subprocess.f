module subprocess_handler
    use, intrinsic :: iso_c_binding

    implicit none

    public  :: process_start
    !public  :: process_close
    !public  :: process_readline
    !public  :: process_kill

    logical(c_bool), parameter :: logical_false = .false.
    logical(c_bool), parameter :: logical_true = .true.
    integer, parameter :: BUFFER_SIZE = 4096

    type, public       :: handle_pointer
        type(c_ptr)    :: handle = c_null_ptr
    end type

    interface
        integer(c_int) function subprocess_create_c(command_line, options, out_process) bind(C, name='subprocess_create')
            use, intrinsic :: iso_c_binding
            character(c_char), dimension(*) :: command_line
            integer(c_int), intent(in), value :: options
            type(c_ptr), intent(out) :: out_process
        end function
        
        integer(c_int) function subprocess_create_ex_c(command_line, options, environment, out_process) bind(C, name='subprocess_create_ex')
            use, intrinsic :: iso_c_binding
            character(c_char), dimension(*) :: command_line
            integer(c_int), intent(in), value :: options
            character(c_char), dimension(*) :: environment
            type(c_ptr), intent(out) :: out_process
        end function
    
        type(c_ptr) function subprocess_stdin_c(process) bind(C, name='subprocess_stdin')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
        end function
    
        type(c_ptr) function subprocess_stdout_c(process) bind(C, name='subprocess_stdout')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
        end function
    
        type(c_ptr) function subprocess_stderr_c(process, out_return_code) bind(C, name='subprocess_stderr')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
            integer(c_int), intent(out) :: out_return_code
        end function
    
        integer(c_int) function subprocess_join_c(process) bind(C, name='subprocess_join')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
        end function
    
        integer(c_int) function subprocess_destroy_c(process) bind(C, name='subprocess_destroy')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
        end function
    
        integer(c_int) function subprocess_terminate_c(process) bind(C, name='subprocess_terminate')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
        end function
    
        integer(c_int) function subprocess_read_stdout_c(process, buffer, size) bind(C, name='subprocess_read_stdout')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
            character(c_char), dimension(*) :: buffer
            integer(c_int), intent(in), value :: size
        end function
    
        integer(c_int) function subprocess_read_stderr_c(process, buffer, size) bind(C, name='subprocess_read_stderr')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: process
            character(c_char), dimension(*) :: buffer
            integer(c_int), intent(in), value :: size
        end function
    
        type(c_ptr) function fgets_c(str, numChars, stream) bind(C, name='fgets')
            use, intrinsic :: iso_c_binding
            character(c_char), intent(inout) :: str(*)
            integer(c_int), intent(out) :: numChars
            type(c_ptr), value :: stream
        end function

        function fputs_c(buf, handle) bind(C, name='fputs')
            use, intrinsic :: iso_c_binding
            integer(c_int) :: fputs_c
            character(c_char), dimension(*) :: buf
            type(c_ptr), value :: handle
        end function

        function fflush_c(handle) bind(C, name='fflush')
            use, intrinsic :: iso_c_binding
            integer(c_int) :: fflush_c
            type(c_ptr), value :: handle
        end function
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
    integer function process_start(cmd, mode, fp, ierr) result(pid)
        character(*), intent(in)                :: cmd
        character(*), intent(in)                :: mode
        type(handle_pointer), intent(out)       :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer                                 :: current_pid
        integer :: istat

        istat = 0
        if (present(ierr)) ierr = istat

        pid = subprocess_create_c(cmd//c_null_char, 0, fp%handle)
        if (.not. c_associated(fp%handle)) then
            write (*, *) '*process_start* ERROR: Could not open pipe!'
            istat = -1
        end if
        if (present(ierr)) ierr = istat
    end function

end module
