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

    implicit none; private

    public :: kill, &
              run, &
              runasync, &
              read_stderr, &
              read_stdout, &
              wait, &
              waitall, &
              process_io

    type, public :: process
        private
        integer, public                     :: pid
        character(:), allocatable, public   :: filename
        integer                             :: excode
        real(r8)                            :: begtime
        real(r8)                            :: extime
        logical                             :: is_running
        type(c_funptr)                      :: stdout = c_null_funptr
        type(c_funptr)                      :: stderr = c_null_funptr
        type(handle_pointer)                :: ptr
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
        
        generic, public :: run => process_run_default, &
                                  process_run_with_arg1, &
                                  process_run_with_arg2, &
                                  process_run_with_arg3, &
                                  process_run_with_arg4, &
                                  process_run_with_arg5, &
                                  process_run_with_args
        generic, public :: runasync => process_runasync_default, &
                                       process_runasync_with_arg1, &
                                       process_runasync_with_arg2, &
                                       process_runasync_with_arg3, &
                                       process_runasync_with_arg4, &
                                       process_runasync_with_arg5, &
                                       process_runasync_with_args
        procedure, pass(this), public       :: read_stdout => process_read_stdout 
        procedure, pass(this), public       :: read_stderr => process_read_stderr 
        procedure, pass(this), public       :: wait => process_wait
        procedure, pass(this), public       :: kill => process_kill
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
    
    type :: string
        private
        character(:), allocatable, public :: chars
    contains
        procedure, private, pass(lhs) :: string_assign_character
        generic :: assignment(=)      => string_assign_character
    end type

    interface trim
        module procedure :: string_trim
    end interface
    
    abstract interface 
        subroutine process_io(sender, msg)
            import
            type(process), intent(in)   :: sender
            character(*), intent(in)    :: msg
        end subroutine
    end interface

contains

    function process_new(name, stdin, stdout, stderr) result(that)
        character(*), intent(in)        :: name
        procedure(process_io), intent(out), pointer, optional   :: stdin
        procedure(process_io), optional                :: stdout
        procedure(process_io), optional                :: stderr
        type(process) :: that
        
        call internal_finalize(that%ptr)
        
        that%is_running = .false.
        that%excode = 0
        that%filename = trim(name)
        if (present(stdin)) stdin => process_writeto_stdin
        if (present(stdout)) that%stdout = c_funloc(stdout)
        if (present(stderr)) that%stderr = c_funloc(stderr)
    end function
   
    subroutine process_run_default(this)
        class(process), intent(inout)   :: this !< process object type
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_run_with_args(this, args)
    end subroutine
    
    subroutine process_run_with_arg1(this, arg1)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        !private
        type(string) :: args

        args = arg1
        call process_run_with_args(this, [args])
    end subroutine
    
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
    
    subroutine process_run_with_args(this, args)
        class(process), intent(inout)   :: this !< process object type
        type(string), intent(in)        :: args(:)
        !private
        character(:), allocatable :: cmd
        procedure(process_io), pointer :: fptr => null()
        integer :: i

        cmd = this%filename
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do

        this%excode = 0
        this%is_running = .true.
        
        call get_time(this%begtime)
        this%pid = internal_run(cmd, this%ptr, this%excode)
        call get_time(this%extime)
        this%is_running = internal_isalive(this%ptr)
        
        if (c_associated(this%stdout)) then
            call c_f_procpointer(this%stdout, fptr)
            call fptr(this, internal_read_stdout(this%ptr))
            nullify(fptr)
        end if
        
        if (c_associated(this%stderr)) then
            call c_f_procpointer(this%stderr, fptr)
            call fptr(this, internal_read_stderr(this%ptr))
            nullify(fptr)
        end if
    end subroutine
    
        subroutine process_runasync_default(this)
        class(process), intent(inout)   :: this !< process object type
        !private
        type(string), allocatable :: args(:)
        
        allocate(args(0))
        call process_runasync_with_args(this, args)
    end subroutine
    
    subroutine process_runasync_with_arg1(this, arg1)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        !private
        type(string) :: args

        args = arg1
        call process_runasync_with_args(this, [args])
    end subroutine
    
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
    
    subroutine process_runasync_with_arg3(this, arg1, arg2, arg3)
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
        call process_runasync_with_args(this, args)
    end subroutine
    
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
    
    subroutine process_runasync_with_args(this, args)
        class(process), intent(inout)   :: this !< process object type
        type(string), intent(in)        :: args(:)
        !private
        character(:), allocatable :: cmd
        integer :: i
        
        cmd = this%filename
        do i = 1, size(args)
            cmd = trim(cmd)//' '//trim(args(i))
        end do
        
        this%excode = 0
        this%is_running = .true.
        call get_time(this%begtime)
        this%pid = internal_runasync(cmd, this%ptr, this%excode)
    end subroutine
    
    !> @brief Gets the value that the associated process specified when 
    !! it terminated.
    function process_exit_code(this) result(res)
        class(process), intent(inout)   :: this !< process object type
        integer :: res
        
        res = this%excode
    end function
    
    !> @brief Gets the time that the associated process exited.
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
    
    !> @brief Gets a value indicating whether the associated process has 
    !! been terminated.
    function process_has_exited(this) result(res)
        class(process), intent(inout)   :: this !< process object type
        logical :: res
        
        this%is_running = internal_isalive(this%ptr)
        res = .not. this%is_running
    end function
    
    subroutine process_wait(this)
        class(process), intent(inout) :: this
        
        call internal_wait(this%ptr, this%excode)
        this%is_running = internal_isalive(this%ptr)
    end subroutine
    
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
    
    subroutine process_read_stdout(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stdout(this%ptr)
    end subroutine
    
    subroutine process_read_stderr(this, output)
        class(process), intent(inout)           :: this
        character(:), allocatable, intent(out)  :: output
        
        output = internal_read_stderr(this%ptr)
    end subroutine
    
    subroutine process_writeto_stdin(sender, msg)
        type(process), intent(in)   :: sender
        character(*), intent(in)    :: msg
        
        call internal_writeto_stdin(sender%ptr, msg)
    end subroutine
    
    subroutine finalize(this)
        type(process), intent(inout) :: this
        
        call internal_finalize(this%ptr)
        this%is_running = .false.
    end subroutine
  
    subroutine get_time(ctime) 
        real(r8), intent(out)           :: ctime !< time in milliseconds
        !private
        integer(i8) :: dt(8)

        call date_and_time(values=dt)
        ctime = (dt(5) * 3600_r8 + dt(6) * 60_r8 + dt(7)) * 1000_r8 + dt(8) * 1_r8
    end subroutine

    pure subroutine string_assign_character(lhs, rhs)
        class(string), intent(inout) :: lhs
        character(*), intent(in)     :: rhs

        lhs%chars = rhs
    end subroutine

    function string_trim(str) result(res)
        class(string), intent(in) :: str
        character(:), allocatable :: res
        
        res = trim(str%chars)
    end function

end module
