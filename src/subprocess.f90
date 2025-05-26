!> @defgroup group_subprocess Subprocess
!! @ingroup group_api
!! @brief @link subprocess::process Subprocess @endlink module
!> @cond
#ifdef _WIN32
#define PATH_MAX 255
#define MAX_ARG_STRLEN 8191
#else
#define PATH_MAX 4095
#define MAX_ARG_STRLEN 131071
#endif
!> @endcond
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
              sleep, &
              waitall, &
              writeto, &
              process_io

    public :: option_none,                     &
              option_combined_stdout_stderr,   &
              option_inherit_environment,      &
              option_no_window,                &
              option_search_user_path,         &
              option_enum

    !> @class process
    !! @ingroup group_subprocess
    !> @brief   A derived type representing a subprocess with associated properties and methods.
    !!          This type encapsulates the state and behavior of a subprocess, including its process ID,
    !!          path, execution status, and I/O handlers. It provides methods to run, manage, and
    !!          interact with the subprocess.
    !! <h2>Examples</h2>
    !! The following examples demonstrate some of the main members of the @ref process. 
    !! @n
    !! The first example shows a simple usage of the process type. It creates a child process
    !! to list the files in a given folder. 
    !! @n
    !! @snippet snippet.f90 process_ex1
    !! @n
    !! The second example demonstrates how to run a process asynchronously. The standard input
    !! is redirected to a procedure pointer returned from the constructor.
    !! @n
    !! @snippet snippet.f90 process_ex2
    !! @n
    !! The third example demonstrate how functions can be passed to the process constructor
    !! to redirect the standard output.
    !! @n
    !! @snippet snippet.f90 process_ex3
    !! @par
    !! <h2>Constructors</h2>
    !! Constructs a new process object with the specified path 
    !! and optional I/O handlers.
    !! <h3>process(character(*), procedure(process_io), procedure(process_io), procedure(process_io))</h3>
    !! @verbatim type(process) function process(character(*) path) @endverbatim
    !!
    !! @param[in] path The path or command to execute.
    !! @param[out] stdin Optional pointer to a procedure for handling stdin.
    !! @param[in] stdout Optional procedure for handling stdout.
    !! @param[in] stderr Optional procedure for handling stderr.
    !!
    !! @b Examples
    !! ```fortran
    !! type(process) :: p
    !! p = process('cmd') 
    !! ```
    !! @return The constructed process object.
    !!
    !! @b Remarks
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
        !> @brief Logical flag indicating id the subprocess is running asynchronously
        logical                                         :: isasync
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
        procedure, pass(this), public       :: exit_code => process_exit_code
        procedure, pass(this), public       :: exit_time => process_exit_time
        procedure, pass(this), public       :: has_exited => process_has_exited
        generic, public :: run              => process_run_default,             &
                                                process_run_with_arg1,           &
                                                process_run_with_arg2,           &
                                                process_run_with_arg3,           &
                                                process_run_with_arg4,           &
                                                process_run_with_arg5,           &
                                                process_run_with_args
        generic, public :: runasync         => process_runasync_default,        &
                                                process_runasync_with_arg1,      &
                                                process_runasync_with_arg2,      &
                                                process_runasync_with_arg3,      &
                                                process_runasync_with_arg4,      &
                                                process_runasync_with_arg5,      &
                                                process_runasync_with_args
        procedure, pass(this), public       :: read_stdout => process_read_stdout 
        procedure, pass(this), public       :: read_stderr => process_read_stderr 
        procedure, pass(this), public       :: wait => process_wait
        procedure, pass(this), public       :: kill => process_kill
        final :: finalize
    end type

    
    interface process
    !! @cond
        module procedure :: process_new
    !! @endcond
    end interface
    
    !> @interface kill
    !! @ingroup group_subprocess
    !> @brief Terminates the associated process.
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>kill(type(process) p)</h3>
    !! 
    !! @param[in,out] this The process object to terminate.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `kill` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  p = process('notepad')
    !!
    !!  p%runasync()
    !!  call kill(p)
    !! @endcode
    !! <h2> Remarks </h2>
    interface kill
        module procedure :: process_kill
    end interface

    !> @interface run
    !! @ingroup group_subprocess
    !> @brief Runs a process synchronously.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>run(type(process) p, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, character(*) arg1, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, character(*) arg1, character(*) arg2, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, character(*) arg4, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, character(*) arg4, character(*) arg5, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>run(type(process) p, type(string) args, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run.
    !! @param[in] args Array of arguments to pass to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `run` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  p = process('ls')
    !!  call run(p, './ *.f90')
    !! @endcode
    !! <h2> Remarks </h2>
    interface run
        module procedure :: process_run_default, &
                            process_run_with_arg1, &
                            process_run_with_arg2, &
                            process_run_with_arg3, &
                            process_run_with_arg4, &
                            process_run_with_arg5, &
                            process_run_with_args
    end interface
    
    !> @interface runasync
    !! @ingroup group_subprocess
    !> @brief Runs a process asynchronously.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>runasync(type(process) p, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, character(*) arg1, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run  asynchronously.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, character(*) arg1, character(*) arg2, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, character(*) arg4, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, character(*) arg1, character(*) arg2, character(*) arg3, character(*) arg4, character(*) arg5, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> </h2>
    !! <h3>runasync(type(process) p, type(string) args, (optional) integer option)</h3>
    !! 
    !! @param[in,out] this The process object to run asynchronously.
    !! @param[in] args Array of arguments to pass to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `runasync` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  p = process('ls')
    !!  call runasync(p, './ *.f90')
    !!  !...
    !!  call wait(p)
    !! @endcode
    !! <h2> Remarks </h2>
    interface runasync
        module procedure :: process_runasync_default, &
                            process_runasync_with_arg1, &
                            process_runasync_with_arg2, &
                            process_runasync_with_arg3, &
                            process_runasync_with_arg4, &
                            process_runasync_with_arg5, &
                            process_runasync_with_args
    end interface
    
    !> @interface wait
    !! @ingroup group_subprocess
    !> @brief Waits for the associated process to complete.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>wait(type(process) p)</h3>
    !! 
    !! @param[in,out] this The process object to wait for.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `wait` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  p = process('ls')
    !!  call runasync(p, './ *.f90')
    !!  !...
    !!  call wait(p)
    !! @endcode
    !! <h2> Remarks </h2>
    interface wait 
        module procedure :: process_wait
    end interface
    
    !> @interface waitall
    !! @ingroup group_subprocess
    !> @brief Waits for all processes in an array to complete.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>waitall(type(process) p(:))</h3>
    !! 
    !! @param[in,out] processes Array of process objects to wait for.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `waitall` interface.
    !! @code{.f90}
    !!  type(process) :: p(5)
    !!  integer :: i
    !!
    !!  do i = 1, 5
    !!      p(i) = p = process('long_running_process')
    !!      call p(i)%run()
    !!  end do
    !!  call waitall(p)
    !! @endcode
    !! <h2> Remarks </h2>
    interface waitall
        module procedure :: process_waitall
    end interface
    
    !> @interface read_stdout
    !! @ingroup group_subprocess
    !> @brief Reads the standard output of the associated process.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>read_stdout(type(process) p(:), character(:), allocatable msg)</h3>
    !! 
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stdout as a character string.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `read_stdout` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  character(:), allocatable :: files
    !!
    !!  type(process) :: p
    !!  p = process('ls')
    !!  call run(p, './ *.f90')
    !!  call read_stdout(p, files)
    !!
    !!  print *, trim(files(i))
    !! @endcode
    !! <h2> Remarks </h2>
    interface read_stdout
        module procedure :: process_read_stdout
    end interface
    
    !> @interface read_stderr
    !! @ingroup group_subprocess
    !> @brief Reads the standard error of the associated process.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>read_stderr(type(process) p(:), character(:), allocatable output)</h3>
    !! 
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stderr as a character string.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `read_stderr` interface.
    !! It uses the return code from the `exit_code` function to determine whether 
    !! or not an error occurred during execution.
    !! @code{.f90}
    !!  type(process) :: p
    !!  character(:), allocatable :: errmsg
    !!
    !!  type(process) :: p
    !!  p = process('ls')
    !!  call run(p, './ *.f90')
    !!  call read_stderr(p, errmsg)
    !!
    !!  if (p%exit_code() /= 0) then
    !!      call read_stderr(p, errmsg)   
    !!      print *, errmsg
    !!  end if
    !! @endcode
    !! <h2> Remarks </h2>
    interface read_stderr
        module procedure :: process_read_stderr
    end interface

    !> @interface writeto
    !! @ingroup group_subprocess
    !> @brief Write the standard inlet of the associated process.
    !!
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>writeto(type(process) p(:), character(:), allocatable output)</h3>
    !! 
    !! @param[in,out] this The process object.
    !! @param[out] output The message to write to stdin.
    !!
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `writeto` interface.
    !! @code{.f90}
    !!  type(process) :: p
    !!  character(*) :: msg = 'Hello World!'
    !!
    !!  type(process) :: p
    !!  p = process('my_process')
    !!  call runasync(p)
    !!  call writeto(p, msg)
    !!
    !!  call wait(p)
    !! @endcode
    !! <h2> Remarks </h2>
    interface writeto
        module procedure :: process_writeto_stdin
    end interface
    
    abstract interface 
        !! @ingroup group_subprocess
        !> @brief Abstract interface for io procedures.
        !! @param[in] sender The process to write to or read from
        !! @param[in] msg The message to exchange with the running process   
        subroutine process_io(sender, msg)
            import
            implicit none
            class(process), intent(in)  :: sender
            character(*), intent(in)    :: msg
        end subroutine
    end interface

contains

    function process_new(path, stdin, stdout, stderr) result(that)
        character(*), intent(in)                                :: path
        procedure(process_io), intent(out), pointer, optional   :: stdin
        procedure(process_io), optional                         :: stdout
        procedure(process_io), optional                         :: stderr
        type(process) :: that
        
        call internal_finalize(that%ptr)
        
        that%is_running = .false.
        that%path = trim(path)
        that%isasync = .false.
        if (present(stdin)) then
            nullify(stdin)
            stdin => process_writeto_stdin
        end if
        if (present(stdout)) that%stdout => stdout
        if (present(stderr)) that%stderr => stderr
    end function
   
    !> @brief Runs a process synchronously with no arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_default(this, option)
        class(process), intent(inout)   :: this
        integer(option_enum), intent(in), optional :: option
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_run_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process synchronously with one argument.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_arg1(this, arg1, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args

        args = arg1
        call process_run_with_args(this, [args], option)
    end subroutine
    
    !> @brief Runs a process synchronously with two arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_arg2(this, arg1, arg2, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(2)

        args(1) = arg1
        args(2) = arg2
        call process_run_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process synchronously with three arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_arg3(this, arg1, arg2, arg3, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(3)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        call process_run_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process synchronously with four arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_arg4(this, arg1, arg2, arg3, arg4, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(4)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        call process_run_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process synchronously with five arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_arg5(this, arg1, arg2, arg3, arg4, arg5, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        character(*), intent(in)        :: arg5
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(5)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        args(5) = arg5
        call process_run_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process synchronously with an array of arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] args Array of arguments to pass to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_run_with_args(this, args, option)
        class(process), intent(inout)   :: this
        type(string), intent(in)        :: args(:)
        integer(option_enum), intent(in), optional :: option
        !private
        character(:), allocatable :: cmd, arg
        procedure(process_io), pointer :: fptr => null()
        integer :: i

        this%isasync = .false.
        cmd = this%path
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do

        if (allocated(this%excode)) deallocate(this%excode)
        allocate(this%excode, source = 0)
        
        this%is_running = .true.
        
        call get_time(this%begtime)
        if (present(option)) then
            this%pid = internal_run(cmd, option, this%ptr, this%excode)
        else
            this%pid = internal_run(cmd, this%ptr, this%excode)
        end if
        call get_time(this%extime)
        this%is_running = internal_isalive(this%ptr)
        
        if (associated(this%stdout)) then
            call this%stdout(this, internal_read_stdout(this%ptr, this%isasync))
        end if
        
        if (associated(this%stderr)) then
            call this%stderr(this, internal_read_stderr(this%ptr, this%isasync))
        end if
    end subroutine
    
    !> @brief Runs a process asynchronously with no arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_default(this, option)
        class(process), intent(inout)   :: this
        integer(option_enum), intent(in), optional :: option
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_runasync_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process asynchronously with one argument.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink 
    !!
    !! @b Remarks
    subroutine process_runasync_with_arg1(this, arg1, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args

        args = arg1
        call process_runasync_with_args(this, [args], option)
    end subroutine
    
    !> @brief Runs a process asynchronously with two arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_with_arg2(this, arg1, arg2, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(2)

        args(1) = arg1
        args(2) = arg2
        call process_runasync_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process asynchronously with three arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_with_arg3(this, arg1, arg2, arg3, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(3)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        call process_runasync_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process asynchronously with four arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_with_arg4(this, arg1, arg2, arg3, arg4, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(4)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        call process_runasync_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process asynchronously with five arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] arg1 The first argument to the process.
    !! @param[in] arg2 The second argument to the process.
    !! @param[in] arg3 The third argument to the process.
    !! @param[in] arg4 The fourth argument to the process.
    !! @param[in] arg5 The fifth argument to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_with_arg5(this, arg1, arg2, arg3, arg4, arg5, option)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        character(*), intent(in)        :: arg3
        character(*), intent(in)        :: arg4
        character(*), intent(in)        :: arg5
        integer(option_enum), intent(in), optional :: option
        !private
        type(string) :: args(5)

        args(1) = arg1
        args(2) = arg2
        args(3) = arg3
        args(4) = arg4
        args(5) = arg5
        call process_runasync_with_args(this, args, option)
    end subroutine
    
    !> @brief Runs a process asynchronously with an array of arguments.
    !!
    !! @param[in,out] this The process object to run.
    !! @param[in] args Array of arguments to pass to the process.
    !! @param[in] option An integer option selected from @link subprocess_handler::option_enum option_enum@endlink
    !!
    !! @b Remarks
    subroutine process_runasync_with_args(this, args, option)
        class(process), intent(inout)   :: this
        type(string), intent(in)        :: args(:)
        integer(option_enum), intent(in), optional :: option
        !private
        character(:), allocatable :: cmd, arg
        integer :: i
        
        this%isasync = .true.
        cmd = this%path
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do
        
        if (allocated(this%excode)) deallocate(this%excode)
        allocate(this%excode, source = 0)
        this%is_running = .true.
        call get_time(this%begtime)
        if (present(option)) then
            this%pid = internal_runasync(cmd, option, this%ptr, this%excode)
        else
            this%pid = internal_runasync(cmd, this%ptr, this%excode)
        end if
        if (this%excode == 0) then 
            deallocate(this%excode)
        end if
    end subroutine
    
    !> @brief Gets the exit code of the associated process.
    !!
    !! @param[in,out] this The process object.
    !!
    !! @return The exit code of the process, or 0 if exited successfully, or 383 (0x017F) if still running.
    !!
    !! @b Remarks
    function process_exit_code(this) result(res)
        class(process), intent(inout)   :: this
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
    !!
    !! @param[in,out] this The process object.
    !!
    !! @return The elapsed time in milliseconds.
    !!
    !! @b Remarks
    function process_exit_time(this) result(res)
        class(process), intent(inout)   :: this
        real(r8) :: res
        
        if (this%has_exited()) then
            res = this%extime - this%begtime
        else 
            call get_time(res)
            res = res - this%begtime
        end if
    end function
    
    !> @brief Checks if the associated process has terminated.
    !!
    !! @param[in,out] this The process object.
    !!
    !! @return True if the process has exited, false otherwise.
    !!
    !! @b Remarks
    function process_has_exited(this) result(res)
        class(process), intent(inout)   :: this
        logical :: res
        
        this%is_running = internal_isalive(this%ptr)
        res = .not. this%is_running
    end function
    
    !> @brief Waits for the associated process to complete.
    !!
    !! @param[in,out] this The process object to wait for.
    !!
    !! @b Remarks
    subroutine process_wait(this)
        class(process), intent(inout) :: this
        
        call internal_wait(this%ptr, this%excode)
        this%is_running = internal_isalive(this%ptr)
    end subroutine
    
    !> @brief Waits for all processes in an array to complete.
    !!
    !! @param[in,out] processes Array of process objects to wait for.
    !!
    !! @b Remarks
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
    !!
    !! @param[in,out] this The process object to terminate.
    !!
    !! @b Remarks
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
        this%isasync = .false.
        call internal_finalize(this%ptr)
    end subroutine
    
    !> @brief Reads the standard output of the associated process.
    !!
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stdout as a character string.
    !!
    !! @b Remarks
    subroutine process_read_stdout(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stdout(this%ptr, this%isasync)
    end subroutine
    
    !> @brief Reads the standard error of the associated process.
    !!
    !! @param[in,out] this The process object.
    !! @param[out] output The captured stderr as a character string.
    !!
    !! @b Remarks
    subroutine process_read_stderr(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stderr(this%ptr, this%isasync)
    end subroutine
    
    !> @brief Writes a message to the standard input of the associated process.
    !!
    !! @param[in] sender The process object sending the message.
    !! @param[in] msg The message to write to stdin.
    !!
    !! @b Remarks
    subroutine process_writeto_stdin(sender, msg)
        class(process), intent(in)  :: sender
        character(*), intent(in)    :: msg
        
        call internal_writeto_stdin(sender%ptr, msg)
    end subroutine
    
    !> @brief Finalizes the process object, releasing resources.
    !!
    !! @param[in,out] this The process object to finalize.
    !!
    !! @b Remarks
    subroutine finalize(this)
        type(process), intent(inout) :: this

        nullify(this%stdout)
        nullify(this%stdout)
        
        this%isasync = .false.
        call internal_finalize(this%ptr)
        this%is_running = .false.
    end subroutine
  
    !> @brief Gets the current time in milliseconds.
    !!
    !! @param[out] ctime The current time in milliseconds.
    !!
    !! @b Remarks
    subroutine get_time(ctime) 
        real(r8), intent(out) :: ctime !< time in milliseconds
        !private
        integer(i8) :: dt(8)

        call date_and_time(values=dt)
        ctime = (dt(5) * 3600_r8 + dt(6) * 60_r8 + dt(7)) * 1000_r8 + dt(8) * 1_r8
    end subroutine

end module
