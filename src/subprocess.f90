module subprocess
    use subprocess_handler

    implicit none; private

    type, public :: process
        private
        integer :: pid
        character(:), allocatable, public   :: command
        character(:), allocatable, public   :: output
        type(string), allocatable, private  :: args(:)
        type(handle_pointer), private       :: ptr
        logical                             :: closable
    contains
        procedure, pass(this), public       :: with_arg
        procedure, pass(this), public       :: run => process_run
        procedure, pass(this), public       :: wait => process_wait
        procedure, pass(this), public       :: kill => process_kill
        procedure, pass(lhs), private :: process_assign
		generic :: assignment(=) => process_assign
        final :: finalize
    end type

    interface process
        module procedure :: process_new
    end interface
    
    interface run
        module procedure :: process_run
    end interface
    
    interface wait 
        module procedure :: process_wait
    end interface
    
    type :: string
        character(:), allocatable :: chars
    end type

contains

    type(process) function process_new(prog) result(that)
        character(*), intent(in) :: prog

        call internal_finalize(that%ptr)
        
        if (allocated(that%args)) deallocate (that%args)
        that%command = trim(prog)
        that%closable = .true.
    end function

    subroutine with_arg(this, arg)
        class(process), intent(inout) :: this
        character(*), intent(in) :: arg
        type(string) :: vs

        vs%chars = arg
        if (allocated(this%args)) then
            this%args = [this%args, vs]
        else
            allocate (this%args(1))
            this%args(1)%chars = arg
        end if
    end subroutine

    subroutine process_run(this, success, code)
        class(process), intent(inout)   :: this !< process object type
        logical, intent(out), optional  :: success !< optional output parameter. `success` equals .true. when the command was successfull
        integer, intent(out), optional  :: code !< optional output parameter. `code` is the return code of the process
        !private
        character(:), allocatable :: cmd, line
        integer i

        cmd = this%command
        if (allocated(this%args)) then
            do i = 1, size(this%args)
                cmd = trim(cmd)//" "//trim(this%args(i)%chars)
            end do
        end if

        success = .true.
        code = 0

        this%pid = internal_run(cmd, 0, this%ptr, code)

        this%output = internal_read_stdout(this%ptr)
        call internal_finalize(this%ptr)
    end subroutine
    
    subroutine process_wait(this)
        class(process), intent(inout) :: this
        
        call internal_wait(this%ptr)
    end subroutine

    subroutine process_kill(this)
        class(process), intent(inout) :: this

        !call process_kill(this%pid)
    end subroutine
    
    subroutine finalize(this)
        type(process), intent(inout) :: this
        
        if (this%closable) call internal_finalize(this%ptr)
    end subroutine
    
    subroutine process_assign(lhs, rhs)
        use iso_c_binding
		class(process), intent(inout)  :: lhs
		class(process), intent(in)     :: rhs
        !private
        logical :: false
        
        interface
            subroutine memcpy(dest, src, n) bind(c, name='memcpy')
                import
                integer(c_intptr_t), intent(in), value :: dest
                integer(c_intptr_t), intent(in), value :: src
                integer(c_size_t), value :: n
            end subroutine
        end interface
		
        false = .false.
        
		lhs%pid = rhs%pid
        lhs%command = rhs%command
        lhs%output = rhs%output
        lhs%args = rhs%args
        lhs%ptr = rhs%ptr
        lhs%closable = rhs%closable
		!the use of memcpy is necessary to change the value of closable while 
        !rhs is declared as intent in. This is a dirty trick and is therefore 
        !not standard compliant. This method should never be used unless you
        !know exactly what you are doing
        call memcpy(loc(rhs%closable), loc(false), storage_size(rhs%closable, kind=c_size_t)/8_c_size_t)
    end subroutine

end module
