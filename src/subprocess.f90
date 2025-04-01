#ifdef _WIN32
#define PATH_MAX 255
#define MAX_ARG_STRLEN 8191
#else
#define PATH_MAX 4095
#define MAX_ARG_STRLEN 131071
#endif

module subprocess
    use subprocess_handler
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: r8 => real64, i8 => int64
    use subprocess_string

    implicit none; private

    public :: kill, &
              run, &
              runasync, &
              read_stderr, &
              read_stdout, &
              wait, &
              waitall, &
              writeto, &
              process_io

    !> @brief A derived type representing a subprocess with associated properties and methods.
    !! This type encapsulates the state and behavior of a subprocess, including its process ID,
    !! path, execution status, and I/O handlers. It provides methods to run, manage, and
    !! interact with the subprocess.
    !! @note Fields marked as `public` are accessible outside the module, while others are private.
    type, public :: process
        private
        !> @brief The process ID of the subprocess.
        integer, public                                 :: pid
        !> @brief The path or command associated with the subprocess.
        character(:), allocatable, public               :: path
        !> @brief The exit code of the subprocess (allocated when set).
        integer, allocatable                            :: excode
        !> @brief The start time of the subprocess (in milliseconds).
        real(r8)                                        :: begtime
        !> @brief The exit time of the subprocess (in milliseconds).
        real(r8)                                        :: extime
        !> @brief Logical flag indicating if the subprocess is currently running.
        logical                                         :: is_running
        !> @brief C function pointer to the stdout handler (default: null).
        procedure(process_io), pointer, nopass, private :: stdout => null()
        !> @brief C function pointer to the stderr handler (default: null).
        procedure(process_io), pointer, nopass, private :: stderr => null()
        !> @brief Internal handle pointer for subprocess management.
        type(handle_pointer)                            :: ptr
    contains
        private
        procedure, pass(this)               :: process_run_default
        procedure, pass(this)               :: process_run_with_arg1
        procedure, pass(this)               :: process_run_with_arg2
        procedure, pass(this)               :: process_run_with_arg3
        procedure, pass(this)               :: process_run_with_arg4
        procedure, pass(this)               :: process_run_with_arg5
        procedure, pass(this)               :: process_run_with_args
        procedure, pass(this)               :: process_runasync_default
        procedure, pass(this)               :: process_runasync_with_arg1
        procedure, pass(this)               :: process_runasync_with_arg2
        procedure, pass(this)               :: process_runasync_with_arg3
        procedure, pass(this)               :: process_runasync_with_arg4
        procedure, pass(this)               :: process_runasync_with_arg5
        procedure, pass(this)               :: process_runasync_with_args
        !> @brief Gets the exit code of the subprocess.
        procedure, pass(this), public       :: exit_code => process_exit_code
        !> @brief Gets the elapsed time since the subprocess started or until it exited.
        procedure, pass(this), public       :: exit_time => process_exit_time
        !> @brief Checks if the subprocess has terminated.
        procedure, pass(this), public       :: has_exited => process_has_exited
        !> @brief Generic interface to run the subprocess synchronously with varying arguments.
        generic, public :: run              => process_run_default,             &
                                                process_run_with_arg1,           &
                                                process_run_with_arg2,           &
                                                process_run_with_arg3,           &
                                                process_run_with_arg4,           &
                                                process_run_with_arg5,           &
                                                process_run_with_args
        !> @brief Generic interface to run the subprocess asynchronously with varying arguments.
        generic, public :: runasync         => process_runasync_default,        &
                                                process_runasync_with_arg1,      &
                                                process_runasync_with_arg2,      &
                                                process_runasync_with_arg3,      &
                                                process_runasync_with_arg4,      &
                                                process_runasync_with_arg5,      &
                                                process_runasync_with_args
        !> @brief Reads the standard output of the subprocess.
        procedure, pass(this), public       :: read_stdout => process_read_stdout 
        !> @brief Reads the standard error of the subprocess.
        procedure, pass(this), public       :: read_stderr => process_read_stderr 
        !> @brief Waits for the subprocess to complete.
        procedure, pass(this), public       :: wait => process_wait
        !> @brief Terminates the subprocess.
        procedure, pass(this), public       :: kill => process_kill
        !> @brief Finalizer to release resources when the subprocess object is destroyed.
        final :: finalize
    end type

    interface process
        module procedure :: process_new
    end interface
    
    interface kill
        module procedure :: process_kill
    end interface

    interface run
        module procedure :: process_run_default, &
                            process_run_with_arg1, &
                            process_run_with_arg2, &
                            process_run_with_arg3, &
                            process_run_with_arg4, &
                            process_run_with_arg5, &
                            process_run_with_args
    end interface
    
    interface runasync
        module procedure :: process_runasync_default, &
                            process_runasync_with_arg1, &
                            process_runasync_with_arg2, &
                            process_runasync_with_arg3, &
                            process_runasync_with_arg4, &
                            process_runasync_with_arg5, &
                            process_runasync_with_args
    end interface
    
    interface wait 
        module procedure :: process_wait
    end interface
    
    interface waitall
        module procedure :: process_waitall
    end interface
    
    interface read_stdout
        module procedure :: process_read_stdout
    end interface
    
    interface read_stderr
        module procedure :: process_read_stderr
    end interface

    interface writeto
        module procedure :: process_writeto_stdin
    end interface
        
    abstract interface 
        subroutine process_io(sender, msg)
            import
            implicit none
            type(process), intent(in)   :: sender
            character(*), intent(in)    :: msg
        end subroutine
    end interface

contains

    !> @brief Constructs a new process object with the specified path and optional I/O handlers.
    !! @param[in] path The path or command to execute.
    !! @param[out] stdin Optional pointer to a procedure for handling stdin.
    !! @param[in] stdout Optional procedure for handling stdout.
    !! @param[in] stderr Optional procedure for handling stderr.
    !! @return that The constructed process object.
    function process_new(path, stdin, stdout, stderr) result(that)
        character(*), intent(in)                                :: path
        procedure(process_io), intent(out), pointer, optional   :: stdin
        procedure(process_io), optional                         :: stdout
        procedure(process_io), optional                         :: stderr
        type(process) :: that
        
        call internal_finalize(that%ptr)
        
        that%is_running = .false.
        that%path = trim(path)
        if (present(stdin)) then
            nullify(stdin)
            stdin => process_writeto_stdin
        end if
        if (present(stdout)) that%stdout => stdout
        if (present(stderr)) that%stderr => stderr
    end function
   
    !> @brief Runs a process synchronously with no arguments.
    !! @param[in,out] this The process object to run.
    subroutine process_run_default(this)
        class(process), intent(inout)   :: this !< process object type
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_run_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process synchronously with one argument.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    subroutine process_run_with_arg1(this, arg1)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        !private
        type(string) :: args

        args = arg1
        call process_run_with_args(this, [args])
    end subroutine
    
    !> @brief Runs a process synchronously with two arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    subroutine process_run_with_arg2(this, arg1, arg2)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        !private
        type(string) :: args(2)

        args(1) = arg1
        args(2) = arg2
        call process_run_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process synchronously with three arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    subroutine process_run_with_arg3(this, arg1, arg2, arg3)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        !private
        !private
        type(string) :: args(3)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        call process_run_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process synchronously with four arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    subroutine process_run_with_arg4(this, arg1, arg2, arg3, arg4)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        !private
        !private
        type(string) :: args(4)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        call process_run_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process synchronously with five arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    subroutine process_run_with_arg5(this, arg1, arg2, arg3, arg4, arg5)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        character(*), intent(in)        :: arg5
        !private
        type(string) :: args(5)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        args(5) = arg5
        call process_run_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process synchronously with an array of arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] args Array of arguments to pass to the process.
    subroutine process_run_with_args(this, args)
        class(process), intent(inout)   :: this !< process object type
        type(string), intent(in)        :: args(:)
        !private
        character(:), allocatable :: cmd
        procedure(process_io), pointer :: fptr => null()
        integer :: i

        cmd = this%path
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do

        if (allocated(this%excode)) deallocate(this%excode)
        allocate(this%excode, source = 0)
        
        this%is_running = .true.
        
        call get_time(this%begtime)
        this%pid = internal_run(cmd, this%ptr, this%excode)
        call get_time(this%extime)
        this%is_running = internal_isalive(this%ptr)
        
        if (associated(this%stdout)) then
            call this%stdout(this, internal_read_stdout(this%ptr))
        end if
        
        if (associated(this%stderr)) then
            call this%stderr(this, internal_read_stderr(this%ptr))
        end if
    end subroutine
    
    !> @brief Runs a process asynchronously with no arguments.
    !! @param[in,out] this The process object to run.
    subroutine process_runasync_default(this)
        class(process), intent(inout)   :: this !< process object type
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_runasync_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process asynchronously with one argument.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    subroutine process_runasync_with_arg1(this, arg1)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        !private
        type(string) :: args

        args = arg1
        call process_runasync_with_args(this, [args])
    end subroutine
    
    !> @brief Runs a process asynchronously with two arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    subroutine process_runasync_with_arg2(this, arg1, arg2)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        !private
        type(string) :: args(2)

        args(1) = arg1
        args(2) = arg2
        call process_runasync_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process asynchronously with three arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    subroutine process_runasync_with_arg3(this, arg1, arg2, arg3)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        !private
        type(string) :: args(3)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        call process_runasync_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process asynchronously with four arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    subroutine process_runasync_with_arg4(this, arg1, arg2, arg3, arg4)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        !private
        type(string) :: args(4)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        call process_runasync_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process asynchronously with five arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    subroutine process_runasync_with_arg5(this, arg1, arg2, arg3, arg4, arg5)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        character(*), intent(in)        :: arg5
        !private
        type(string) :: args(5)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        args(5) = arg5
        call process_runasync_with_args(this, args)
    end subroutine
    
    !> @brief Runs a process asynchronously with an array of arguments.
    !! @param[in,out] this The process object to run.
    !! @param[in] args Array of arguments to pass to the process.
    subroutine process_runasync_with_args(this, args)
        class(process), intent(inout)   :: this !< process object type
        type(string), intent(in)        :: args(:)
        !private
        character(:), allocatable :: cmd
        integer :: i
        
        cmd = this%path
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do
        
        if (allocated(this%excode)) deallocate(this%excode)
        allocate(this%excode, source = 0)
        this%is_running = .true.
        call get_time(this%begtime)
        this%pid = internal_runasync(cmd, this%ptr, this%excode)
        if (this%excode == 0) then 
            deallocate(this%excode)
        end if
    end subroutine
    
    !> @brief Gets the exit code of the associated process.
    !! @param[in,out] this The process object.
    !! @return res The exit code of the process, or 0 if exited successfully, or 383 (0x017F) if still running.
    function process_exit_code(this) result(res)
        class(process), intent(inout)   :: this !< process object type
        integer :: res
        
        if (allocated(this%excode)) then
            res = this%excode
        else
            if (this%has_exited()) then
                res = 0
            else
                res = int(z'017F') !the process is still running
            end if
        end if
    end function
    
    !> @brief Gets the elapsed time since the process started or until it exited.
    !! @param[in,out] this The process object.
    !! @return res The elapsed time in milliseconds.
    function process_exit_time(this) result(res)
        class(process), intent(inout)   :: this !< process object type
        real(r8) :: res
        
        if (this%has_exited()) then
            res = this%extime - this%begtime
        else 
            call get_time(res)
            res = res - this%begtime
        end if
    end function
    
    !> @brief Checks if the associated process has terminated.
    !! @param[in,out] this The process object.
    !! @return res True if the process has exited, false otherwise.
    function process_has_exited(this) result(res)
        class(process), intent(inout)   :: this !< process object type
        logical :: res
        
        this%is_running = internal_isalive(this%ptr)
        res = .not. this%is_running
    end function
    
    !> @brief Waits for the associated process to complete.
    !! @param[in,out] this The process object to wait for.
    subroutine process_wait(this)
        class(process), intent(inout) :: this
        
        call internal_wait(this%ptr, this%excode)
        this%is_running = internal_isalive(this%ptr)
    end subroutine
    
    !> @brief Waits for all processes in an array to complete.
    !! @param[in,out] processes Array of process objects to wait for.
    subroutine process_waitall(processes)
        class(process), intent(inout) :: processes(:)
        !private
        integer :: i
        
        do i = 1, size(processes)
            if (.not. processes(i)%has_exited()) then
                call processes(i)%wait()
            end if
        end do
    end subroutine

    !> @brief Terminates the associated process.
    !! @param[in,out] this The process object to terminate.
    subroutine process_kill(this)
        class(process), intent(inout) :: this
        !private
        integer :: ierr

        call internal_terminate(this%ptr, ierr)
        if (ierr == 0) then
            this%is_running = .false.
        else
            this%is_running = internal_isalive(this%ptr)
        end if
        if (.not. this%is_running) call get_time(this%extime)
        call internal_finalize(this%ptr)
    end subroutine
    
    !> @brief Reads the standard output of the associated process.
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stdout as a character string.
    subroutine process_read_stdout(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stdout(this%ptr)
    end subroutine
    
    !> @brief Reads the standard error of the associated process.
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stderr as a character string.
    subroutine process_read_stderr(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stderr(this%ptr)
    end subroutine
    
    !> @brief Writes a message to the standard input of the associated process.
    !! @param[in] sender The process object sending the message.
    !! @param[in] msg The message to write to stdin.
    subroutine process_writeto_stdin(sender, msg)
        type(process), intent(in)   :: sender
        character(*), intent(in)    :: msg
        
        call internal_writeto_stdin(sender%ptr, msg)
    end subroutine
    
    !> @brief Finalizes the process object, releasing resources.
    !! @param[in,out] this The process object to finalize.
    subroutine finalize(this)
        type(process), intent(inout) :: this

        nullify(this%stdout)
        nullify(this%stdout)
        
        call internal_finalize(this%ptr)
        this%is_running = .false.
    end subroutine
  
    !> @brief Gets the current time in milliseconds.
    !! @param[out] ctime The current time in milliseconds.
    subroutine get_time(ctime) 
        real(r8), intent(out) :: ctime !< time in milliseconds
        !private
        integer(i8) :: dt(8)

        call date_and_time(values=dt)
        ctime = (dt(5) * 3600_r8 + dt(6) * 60_r8 + dt(7)) * 1000_r8 + dt(8) * 1_r8
    end subroutine

end module
