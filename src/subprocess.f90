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

    implicit none; private

    public :: kill, &
              run, &
              runasync, &
              read_stderr, &
              read_stdout, &
              wait

    type, public :: process
        private
        integer, public                     :: pid
        character(:), allocatable, public   :: filename
        integer, private                    :: excode
        double precision, private           :: begtime
        double precision, private           :: extime
        logical, private                    :: is_running
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
        procedure, pass(this), public       :: runasync => process_runasync_with_args
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
        module procedure :: process_runasync_with_args
    end interface
    
    interface wait 
        module procedure :: process_wait
    end interface
    
    interface read_stdout
        module procedure :: process_read_stdout
    end interface
    
    interface read_stderr
        module procedure :: process_read_stderr
    end interface
    
    type :: string
        character(:), allocatable :: chars
    end type
    
    abstract interface 
        subroutine process_io(sender, msg)
            class(*), intent(in)        :: sender
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

        args%chars = arg1
        call process_run_with_args(this, [args])
    end subroutine
    
    subroutine process_run_with_arg2(this, arg1, arg2)
        class(process), intent(inout)   :: this
        character(*), intent(in)        :: arg1
        character(*), intent(in)        :: arg2
        !private
        type(string) :: args(2)

        args(1)%chars = arg1
        args(2)%chars = arg2
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

        args(1)%chars = arg1
        args(2)%chars = arg2
        args(3)%chars = arg3
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

        args(1)%chars = arg1
        args(2)%chars = arg2
        args(3)%chars = arg3
        args(4)%chars = arg4
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

        args(1)%chars = arg1
        args(2)%chars = arg2
        args(3)%chars = arg3
        args(4)%chars = arg4
        args(5)%chars = arg5
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
            cmd = trim(cmd)//" "//trim(args(i)%chars)
        end do

        this%excode = 0
        this%is_running = .true.
        this%pid = internal_run(cmd, this%ptr, this%excode)
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
    
    subroutine process_runasync_with_args(this, args)
        class(process), intent(inout)   :: this !< process object type
        type(string), intent(in)        :: args(:)
        !private
        character(:), allocatable :: cmd
        integer :: i
        
        cmd = this%filename
        do i = 1, size(args)
            cmd = trim(cmd)//" "//trim(args(i)%chars)
        end do
        
        this%excode = 0
        this%is_running = .true.
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
        double precision :: res
        
        res = this%excode
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
        class(*), intent(in) :: sender
        character(*), intent(in) :: msg
        
        select type(sender)
        type is (process)
            call internal_writeto_stdin(sender%ptr, msg)
        end select
    end subroutine
    
    subroutine finalize(this)
        type(process), intent(inout) :: this
        
        call internal_finalize(this%ptr)
        this%is_running = .false.
    end subroutine

end module
