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
    contains
        procedure, pass(this), public       :: with_arg
        procedure, pass(this), public       :: run
        procedure, pass(this), public       :: kill
    end type

    interface process
        module procedure :: process_new
    end interface
    
    type :: string
        character(:), allocatable :: chars
    end type

contains

    type(process) function process_new(prog) result(this)
        character(*), intent(in) :: prog

        if (allocated(this%args)) deallocate (this%args)
        this%command = trim(prog)
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

    subroutine run(this, success, code)
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

        this%pid = process_start(cmd, 'r', this%ptr, code)

        !do while (code == 0)
        !    call process_readline(line, this%ptr, code) ! read a line from the process
        !    if (code /= 0) then
        !        exit
        !    end if
        !    this%output = this%output//line
        !end do
        !
        !call process_close(this%ptr, code)
        !if (code /= 0) then
        !    success = .false.
        !end if
    end subroutine

    subroutine kill(this)
        class(process), intent(inout) :: this

        !call process_kill(this%pid)
    end subroutine

end module
