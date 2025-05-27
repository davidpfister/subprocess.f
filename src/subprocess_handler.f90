!> @defgroup group_subprocess_handler Handler
!! @ingroup group_api
!! @brief Provides the binding and wrapper functions to the processlib C library.
!! @par
!! <h2>Remarks</h2>
!! The binding is obtained using the `iso_c_binding` intrinsic module. 
!! If the compiler does not provide such intrinsic module then the 
!! compilation will fail. 
!! @{
module subprocess_handler
    use, intrinsic :: iso_c_binding
    use subprocess_sleep

    implicit none; private

    public  ::  internal_run,           &
                internal_runasync,      &
                internal_finalize,      &
                internal_wait,          &
                internal_writeto_stdin, &
                internal_read_stdout,   &
                internal_read_stderr,   &
                internal_isalive,       &
                internal_terminate,     &
                sleep

    public ::   option_none,                     &
                option_combined_stdout_stderr,   &
                option_inherit_environment,      &
                option_enable_async,             &
                option_no_window,                &
                option_search_user_path,         &
                option_enum
    
    character(*), parameter :: CR = char(13)
    character(*), parameter :: LF = char(10)
    character(*), parameter :: SPACE = char(32) 
    
    !> @brief A C-bound derived type representing the internal state of a subprocess.
    !! This type is used to interface with C functions for managing subprocesses, holding file pointers
    !! for I/O streams and platform-specific process handles. It is not intended for direct user manipulation.
    type, bind(c) :: subprocess_s
        !> @brief Pointer to the subprocess's stdin file stream (FILE* in C).
        type(c_ptr) :: stdin_file 
        !> @brief Pointer to the subprocess's stdout file stream (FILE* in C).
        type(c_ptr) :: stdout_file
        !> @brief Pointer to the subprocess's stderr file stream (FILE* in C).
        type(c_ptr) :: stderr_file
#ifdef _WIN32
        !> @brief Windows-specific handle to the subprocess (void* in C).
        type(c_ptr) :: hProcess
        !> @brief Windows-specific handle to the standard input (void* in C).
        type(c_ptr) :: hStdInput
        !> @brief Windows-specific event handle for output (void* in C).
        type(c_ptr) :: hEventOutput
        !> @brief Windows-specific event handle for error (void* in C).
        type(c_ptr) :: hEventError
#else
        !> @brief POSIX-specific child process ID (pid_t in C).
        integer(c_int) :: child
        !> @brief POSIX-specific return status of the child process.
        integer(c_int) :: return_status
#endif
        !> @brief Flag indicating if the subprocess is alive (non-zero means alive).
        integer(c_int) :: alive
    end type

    integer, parameter :: BUFFER_SIZE = 4095

    !> @brief A derived type wrapping a subprocess handle to manage its lifecycle.
    !! This type encapsulates a `subprocess_s` handle, making it allocatable to avoid gfortran-specific
    !! segmentation faults during finalization (see [Bug 82996](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82996)).
    !! It serves as a Fortran-friendly interface to the C-bound subprocess structure.
    type, public :: handle_pointer
        private
        type(subprocess_s), allocatable :: handle
    end type
    
    !> @class option_enum
    !! @brief Option enumerator type
    !! @li **option_none**                      : default option
    !! @li **option_combined_stdout_stderr**    : stdout and stderr are the same FILE.
    !! @li **option_inherit_environment**       : The child process should inherit the environment variables of the parent.
    !! @li **option_enable_async**              : Enable asynchronous reading of stdout/stderr before it has completed.
    !! @li **option_no_window**                 : Enable the child process to be spawned with no window visible if supported by the platform.
    !! @li **option_search_user_path**          : Search for program names in the PATH variable. Always enabled on Windows. @note 
    !! this will **not** search for paths in any provided custom environment and instead uses the PATH of the spawning process.
    enum, bind(c)
        enumerator :: option_none = 0
        enumerator :: option_combined_stdout_stderr = 1
        enumerator :: option_inherit_environment = 2
        enumerator :: option_enable_async = 4
        enumerator :: option_no_window = 8
        enumerator :: option_search_user_path = 16
    end enum

    
    integer, parameter :: option_enum = kind(option_none)

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

    !> @brief Interface for synchronously running a subprocess.
    !! This generic interface provides a single procedure for executing a command synchronously,
    !! waiting for its completion, and retrieving its exit code.
    interface internal_run
        module procedure :: internal_run_default,           &
                            internal_run_with_options
    end interface
    
    !> @brief Interface for asynchronously running a subprocess.
    !! This generic interface provides a single procedure for executing a command asynchronously,
    !! allowing it to run in the background without waiting for completion.
    interface internal_runasync
        module procedure :: internal_runasync_default,      &
                            internal_runasync_with_options
    end interface
        
contains

    !> @brief Executes a command synchronously and waits for its completion.
    !! This function creates a subprocess, runs the specified command, and waits for it to finish,
    !! returning the process ID and setting the exit code.
    !! @param[in] cmd The command line string to execute (e.g., "ls -l" or "dir").
    !! @param[in,out] fp The handle pointer to manage the subprocess.
    !! @param[out] excode The exit code of the subprocess (0 typically indicates success).
    !! @return pid The process ID of the created subprocess, or a negative value on error.
    integer function internal_run_default(cmd, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr
        
        if (.not. allocated(fp%handle)) allocate(fp%handle)
        pid = subprocess_create_c(cmd // c_null_char, option_inherit_environment, fp%handle)
        if (pid < 0) then
            write (*, *) '*run* ERROR: Could not create process!'
            excode = -1
        end if
        
        ierr = subprocess_join_c(fp%handle, excode)
    end function

    !> @brief Executes a command synchronously and waits for its completion.
    !! This function creates a subprocess, runs the specified command, and waits for it to finish,
    !! returning the process ID and setting the exit code.
    !! @param[in] cmd The command line string to execute (e.g., "ls -l" or "dir").
    !! @param[in] options Integer option
    !! @param[in,out] fp The handle pointer to manage the subprocess.
    !! @param[out] excode The exit code of the subprocess (0 typically indicates success).
    !! @return pid The process ID of the created subprocess, or a negative value on error.
    integer function internal_run_with_options(cmd, options, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        integer(option_enum), intent(in)        :: options(..)
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr, i
        integer(option_enum) :: opt
        
        opt = option_inherit_environment
        select rank(o => options)
        rank(0)
            opt = merge(ior(opt, o), option_none, o /= option_none)
        rank(1)
            do i = 1, size(o)
                if (o(i) == option_none) then
                    opt = option_none
                    exit
                end if
                opt = ior(opt, o(i))
            end do
        rank default
            opt = option_inherit_environment
        end select

        if (.not. allocated(fp%handle)) allocate(fp%handle)
        pid = subprocess_create_c(cmd // c_null_char, opt, fp%handle)
        if (pid < 0) then
            write (*, *) '*run* ERROR: Could not create process!'
            excode = -1
        end if
        
        ierr = subprocess_join_c(fp%handle, excode)
    end function
    
    !> @brief Executes a command asynchronously without waiting for completion.
    !! This function creates a subprocess with asynchronous capabilities and returns immediately,
    !! allowing the command to run in the background.
    !! @param[in] cmd The command line string to execute (e.g., "notepad.exe").
    !! @param[in,out] fp The handle pointer to manage the subprocess.
    !! @param[out] excode The initial exit code (set to -1 if creation fails).
    !! @return pid The process ID of the created subprocess, or a negative value on error.
    function internal_runasync_default(cmd, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: pid
        
        if (.not. allocated(fp%handle)) allocate(fp%handle)
               
        pid = subprocess_create_c(cmd // c_null_char, ior(option_enable_async, option_inherit_environment), fp%handle)
        if (pid < 0) then
            write (*, *) '*runasync* ERROR: Could not create process!'
            excode = -1
        end if
    end function

    !> @brief Executes a command asynchronously without waiting for completion.
    !! This function creates a subprocess with asynchronous capabilities and returns immediately,
    !! allowing the command to run in the background.
    !! @param[in] cmd The command line string to execute (e.g., "notepad.exe").
    !! @param[in] options Integer option  
    !! @param[in,out] fp The handle pointer to manage the subprocess.
    !! @param[out] excode The initial exit code (set to -1 if creation fails).
    !! @return pid The process ID of the created subprocess, or a negative value on error.
    integer function internal_runasync_with_options(cmd, options, fp, excode) result(pid)
        character(*), intent(in)                :: cmd
        integer(option_enum), intent(in)        :: options(..)
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr, i
        integer(option_enum) :: opt
        
        opt = ior(option_enable_async, option_inherit_environment)
        select rank(o => options)
        rank(0)
            opt = merge(ior(opt, o), option_enable_async, o /= option_none)
        rank(1)
            do i = 1, size(o)
                if (o(i) == option_none) then
                    opt = option_enable_async
                    exit
                end if
                opt = ior(opt, o(i))
            end do
        rank default
            opt = ior(option_enable_async, option_inherit_environment)
        end select

        if (.not. allocated(fp%handle)) allocate(fp%handle)
        
        pid = subprocess_create_c(cmd // c_null_char, opt, fp%handle)
        
        if (pid < 0) then
            write (*, *) '*runasync* ERROR: Could not create process!'
            excode = -1
        end if
    end function
    
    !> @brief Writes a message to the standard input of a subprocess.
    !! This subroutine sends the provided message to the stdin of the subprocess if it is active.
    !! @param[in] fp The handle pointer to the subprocess.
    !! @param[in] msg The message to write to stdin.
    subroutine internal_writeto_stdin(fp, msg)
        type(handle_pointer), intent(in) :: fp
        character(*), intent(in)         :: msg
        !private
        integer(c_int) :: ierr
        if (allocated(fp%handle)) then
            if (c_associated(fp%handle%stdin_file)) then
                ierr = fputs_c(trim(msg) // c_null_char, fp%handle%stdin_file)
            end if
        end if
    end subroutine
    
    !> @brief Reads the standard output from a subprocess.
    !! This function captures all available output from the subprocess's stdout, trimming trailing
    !! end-of-line characters based on the platform (CRLF on Windows, LF on others).
    !! @param[in,out] fp The handle pointer to the subprocess.
    !! @param[in] isasync .true. if the process runs asynchronously, .false. overwise.
    !! @return output The captured stdout as an allocatable character string (empty if no output or handle not allocated).
    function internal_read_stdout(fp, isasync) result(output)
        type(handle_pointer), intent(inout)     :: fp
        logical, intent(in)                     :: isasync
        character(:), allocatable :: output
        !private
        integer(c_long) :: l
        character(BUFFER_SIZE) :: buf
#ifdef _WIN32
        character(*), parameter :: eol = CR//LF
#else
        character(*), parameter :: eol = LF
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
                    if (isasync) exit
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
    
    !> @brief Reads the standard error from a subprocess.
    !! This function captures all available error messages from the subprocess's stderr, trimming
    !! trailing end-of-line characters based on the platform (CRLF on Windows, LF on others).
    !! @param[in,out] fp The handle pointer to the subprocess.
    !! @param[in] isasync .true. if the process runs asynchronously, .false. overwise.
    !! @return output The captured stderr as an allocatable character string (empty if no output or handle not allocated).
    function internal_read_stderr(fp, isasync) result(output)
        type(handle_pointer), intent(inout)     :: fp
        logical, intent(in)                     :: isasync
        character(:), allocatable :: output
        !private
        integer(c_long) :: l
        character(BUFFER_SIZE) :: buf
        integer :: n
#ifdef _WIN32
        character(*), parameter :: eol = CR//LF
#else
        character(*), parameter :: eol = LF
#endif
        if (allocated(fp%handle)) then
            l = 1
            buf = ' '
            allocate(character(0)::output)
            do while (l > 0)
                l = subprocess_read_stderr_c(fp%handle, buf, BUFFER_SIZE)
                if (l > 0) then 
                    output = output // trim(buf(:l))
                    if (isasync) exit
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
    
    !> @brief Checks if a subprocess is still running.
    !! This function queries the subprocess's alive status and returns a logical value.
    !! @param[in,out] fp The handle pointer to the subprocess.
    !! @return alive True if the subprocess is still running, false otherwise.
    function internal_isalive(fp) result(alive)
        type(handle_pointer), intent(inout)     :: fp
        logical :: alive
        !private
        integer(c_int) :: status = 0
        if (allocated(fp%handle)) then
            status = subprocess_alive_c(fp%handle)
        end if
        alive = (status /= 0)
    end function
    
    !> @brief Waits for a subprocess to complete and retrieves its exit code.
    !! This subroutine blocks until the subprocess finishes and updates the provided exit code.
    !! @param[in,out] fp The handle pointer to the subprocess.
    !! @param[out] excode The exit code of the subprocess (0 typically indicates success).
    subroutine internal_wait(fp, excode)
        type(handle_pointer), intent(inout)     :: fp
        integer(c_int), intent(out)             :: excode
        !private
        integer :: ierr = 0
        if (allocated(fp%handle)) then
            ierr = subprocess_join_c(fp%handle, excode)
        end if
    end subroutine
    
    !> @brief Terminates a running subprocess.
    !! This subroutine attempts to terminate the subprocess and optionally returns an error status.
    !! @param[in,out] fp The handle pointer to the subprocess.
    !! @param[out] ierr Optional error status (0 indicates success, non-zero indicates failure).
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
    
    !> @brief Finalizes a subprocess by terminating it if running and releasing resources.
    !! This subroutine ensures the subprocess is stopped and its resources are cleaned up.
    !! @param[in,out] fp The handle pointer to the subprocess.
    subroutine internal_finalize(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr
        if (internal_isalive(fp)) call internal_terminate(fp)
        call internal_destroy(fp)
    end subroutine

    !> @brief Destroys a subprocess and deallocates its handle.
    !! This private subroutine releases the subprocess resources and deallocates the handle.
    !! @param[in,out] fp The handle pointer to the subprocess.
    subroutine internal_destroy(fp)
        type(handle_pointer), intent(inout) :: fp
        !private
        integer(c_int) :: ierr = 0
        if (allocated(fp%handle)) then
            ierr = subprocess_destroy_c(fp%handle)
            deallocate(fp%handle)
        end if
    end subroutine
end module
!> @}