module subprocess_handler
    use, intrinsic :: iso_c_binding

    implicit none; private

    public  :: internal_run, &
               internal_runasync, &
               internal_finalize, &
               internal_wait, &
               internal_writeto_stdin, &
               internal_read_stdout, &
               internal_read_stderr, &
               internal_isalive, &
               internal_terminate
    
    type, bind(c) :: subprocess_s
        type(c_ptr) :: stdin_file   !FILE*
		type(c_ptr) :: stdout_file  !FILE*
		type(c_ptr) :: stderr_file  !FILE*
#ifdef _WIN32
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

    ! The component `handle` was made allocatable as a work around with gfortran
    ! If not allocatable, the call the the `finalize` subroutine generates a spurious
    ! SEGFAULT. This issue may (or may not) be related to [Bug 82996](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82996)
    type, public :: handle_pointer
        private
        type(subprocess_s), allocatable :: handle
    end type
    
    enum, bind(c)
        !< stdout and stderr are the same FILE.
        enumerator :: subprocess_option_combined_stdout_stderr = 1
	    !< The child process should inherit the environment variables of the parent.
	    enumerator :: subprocess_option_inherit_environment = 2
	    !< Enable asynchronous reading of stdout/stderr before it has completed.
	    enumerator :: subprocess_option_enable_async = 4
	    !< Enable the child process to be spawned with no window visible if supported
	    !< by the platform.
	    enumerator :: subprocess_option_no_window = 8
	    !< Search for program names in the PATH variable. Always enabled on Windows.
	    !< Note: this will **not** search for paths in any provided custom environment
	    !< and instead uses the PATH of the spawning process.
	    enumerator :: subprocess_option_search_user_path = 16
    end enum

    interface
        integer(c_int) function subprocess_create_c(cmd, options, process) bind(C, name='subprocess_create')
            import
            character(c_char), intent(in) :: cmd(*)
            integer(c_int), intent(in), value :: options
            type(subprocess_s), intent(inout) :: process
        end function
        
        !integer(c_int) function subprocess_create_ex_c(cmd, options, environment, process) bind(C, name='subprocess_create_ex')
        !    import
        !    character(c_char), intent(in) :: cmd(*)
        !    integer(c_int), intent(in), value :: options
        !    character(c_char), dimension(*) :: environment
        !    type(subprocess_s), intent(inout) :: process
        !end function
    
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
    
        integer(c_int) function subprocess_join_c(process, exit_code) bind(C, name='subprocess_join')
            import
            type(subprocess_s), intent(inout) :: process
            integer(c_int), intent(out) :: exit_code
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
    
        integer(c_long) function subprocess_read_stderr_c(process, buffer, size) bind(C, name='subprocess_read_stderr')
            import
            type(subprocess_s), intent(inout) :: process
            character(c_char), intent(inout) :: buffer(*)
            integer(c_long), value :: size
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
            character(c_char), intent(in) :: buf(*)
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
    
    interface internal_runasync
        module procedure :: internal_runasync_default
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
    integer function internal_run_default(cmd, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr
        
        if (.not. allocated(fp%handle)) allocate(fp%handle)
        pid = subprocess_create_c(to_c_string(cmd), 0, fp%handle)
        if (pid < 0) then
            write (*, *) '*process_run* ERROR: Could not create process!'
            excode = -1
        end if
        
        ierr = subprocess_join_c(fp%handle, excode)
    end function
    
    integer function internal_runasync_default(cmd, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr
        if (.not. allocated(fp%handle)) allocate(fp%handle)
        pid = subprocess_create_c(to_c_string(cmd), subprocess_option_enable_async, fp%handle)
        if (pid < 0) then
            write (*, *) '*process_run* ERROR: Could not create process!'
            excode = -1
        end if
    end function
    
    subroutine internal_writeto_stdin(fp, msg)
        type(handle_pointer), intent(in) :: fp
        character(*), intent(in)         :: msg
        !private
        integer(c_int) :: ierr
        if (allocated(fp%handle)) then
            if (c_associated(fp%handle%stdin_file)) then
                ierr = fputs_c(to_c_string(msg), fp%handle%stdin_file)
            end if
        end if
    end subroutine
    
    function internal_read_stdout(fp) result(output)
        type(handle_pointer), intent(inout)     :: fp
        character(:), allocatable :: output
        !private
        integer(c_long) :: l
        character(BUFFER_SIZE) :: buf
#ifdef _WIN32
        character(*), parameter :: eol = char(13)//char(10)
#else
        character(*), parameter :: eol = char(10)
#endif
        integer :: n

        if (allocated(fp%handle)) then
            l = 1
            buf = ' '
            allocate(character(0)::output)
            do while (l > 0)
                l = subprocess_read_stdout_c(fp%handle, buf, BUFFER_SIZE)
                if (l > 0) then 
                    output = output // trim(buf(:l))
                end if
            end do
            n = len(output)-len(eol)
            if (n > 0) then
                if (output(n+1:) == eol) then
                    output = adjustl(output(:n))
                end if
            else
                output = ''
            end if
        else
            output = ''
        end if
    end function
    
    function internal_read_stderr(fp) result(output)
        type(handle_pointer), intent(inout)     :: fp
        character(:), allocatable :: output
        !private
        integer(c_long) :: l
        character(BUFFER_SIZE) :: buf
        integer :: n
#ifdef _WIN32
        character(*), parameter :: eol = char(13)//char(10)
#else
        character(*), parameter :: eol = char(10)
#endif
        if (allocated(fp%handle)) then
            l = 1
            buf = ' '
            allocate(character(0)::output)
            do while (l > 0)
                l = subprocess_read_stderr_c(fp%handle, buf, BUFFER_SIZE)
                if (l > 0) then 
                    output = output // trim(buf(:l))
                end if
            end do
            n = len(output)-len(eol)
            if (n > 0) then
                if (output(n+1:) == eol) then
                    output = adjustl(output(:n))
                end if
            else
                output = ''
            end if
        else 
            output = ''
        end if
    end function
    
    function internal_isalive(fp, ierr) result(alive)
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        logical :: alive
        !private
        integer(c_int) :: status = 0
        if (allocated(fp%handle)) then
            status = subprocess_alive_c(fp%handle)
        end if
        alive = (status /= 0)
    end function
    
    subroutine internal_wait(fp, excode)
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr = 0
        if (allocated(fp%handle)) then
            ierr = subprocess_join_c(fp%handle, excode)
        end if
    end subroutine
    
    subroutine internal_terminate(fp, ierr)
        type(handle_pointer), intent(inout)     :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer :: istat = 0
        integer(c_int) :: rcode
        if (allocated(fp%handle)) then
            istat = subprocess_terminate_c(fp%handle)
        end if
        if (present(ierr)) ierr = istat
    end subroutine
    
    subroutine internal_finalize(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr
        if (internal_isalive(fp)) call internal_terminate(fp)
        call internal_destroy(fp)
    end subroutine

    subroutine internal_destroy(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr = 0
        if (allocated(fp%handle)) then
            ierr = subprocess_destroy_c(fp%handle)
            deallocate(fp%handle)
        end if
    end subroutine
    
    function to_c_string(fstring)
        character(*), intent(in) :: fstring
        character(c_char) :: to_c_string(len(fstring) + 1)
        !private
        integer :: i
        
        do i = 1, len(fstring)
            to_c_string(i) = fstring(i:i)
        end do
        to_c_string(len(fstring) + 1) = c_null_char
    end function

end module
