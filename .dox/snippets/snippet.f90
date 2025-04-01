program snippet

block
!> [process_ex1]
    use subprocess

    type(process) :: p
    character(:), allocatable :: files
    character(*), parameter :: extension = '.f90'
    character(*), parameter :: dirpath = './'

#ifdef _WIN32
    p = process('cmd')
    call run(p, '/c dir "'//dirpath//'" *'//extension)
#else
    p = process('ls')
    call run(p, dirpath//' *'//extension)
#endif

    call read_stdout(p, files)
!> [process_ex1]
end block

block
!> [process_ex2]
    use subprocess

    type(process) :: p
    procedure(process_io), pointer :: stdin => null()

    p = process('process_return_stdin', stdin=stdin)
    call stdin(p, 'a')
    call stdin(p, 'b')
    call stdin(p, 'b')
    call stdin(p, 'a')

    call stdin(p, '@')
    call wait(p)

    print *, p%has_exited()
!> [process_ex2]
end block

block
!> [process_ex3]
    use subprocess

    type(process) :: p

    p = process('hello_world', stdout=write_stdout)
    call run(p)

    contains

    subroutine write_stdout(sender, msg)
        type(process), intent(in)   :: sender
        character(*), intent(in)    :: msg
    
        print*, trim(msg)
    end subroutine

!> [process_ex3]
end block
end program