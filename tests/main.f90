module test_subs
    use subprocess, only: process
    
    implicit none; private
    
    public :: write_stdout, &
              output
    
    character(:), allocatable :: output
    
    contains
    
    subroutine write_stdout(sender, msg)
        type(process), intent(in)   :: sender
        character(*), intent(in)    :: msg
    
        output = trim(msg)
    end subroutine
end module
    
#include <assertion.inc>
TESTPROGRAM(main)

    TEST(test_gfortran)
        use subprocess, only: process, run, read_stderr, read_stdout
        use test_subs

        type(process) :: p1
        character(:), allocatable :: files, file
        character(*), parameter :: extension = '.f90'
        integer :: idx, code, space
        character(:), allocatable :: errmsg
        integer :: prev

#ifndef _FPM
        character(*), parameter :: dirpath = 'TestData/'
        character(*), parameter :: incpath = '../include'
#else
        character(*), parameter :: dirpath = 'tests/TestData/'
        character(*), parameter :: incpath = 'include'
#endif

#ifdef _WIN32
        p1 = process('cmd')
        call run(p1, '/c dir "'//dirpath//'" *'//extension)
#else
        p1 = process('ls')
        call run(p1, dirpath//' *'//extension)
#endif
        EXPECT_TRUE(p1%exit_code() == 0)
        
        call read_stdout(p1, files)
        EXPECT_TRUE(len_trim(files) > 0)
        
        idx = index(files, extension)
        prev = 1
        do
            space = index(files(prev:idx-1), ' ', back=.true.) + prev
            file = files(space:idx-1)
            if (len_trim(file) == 0 .or. file == 'main') exit

            block
                type(process) :: pg
                pg = process('gfortran')
                call run(pg, dirpath//file//extension, '-o '//dirpath//file, '-cpp -I'//incpath)
                code = pg%exit_code()
                EXPECT_TRUE(code == 0)
                if (code /= 0) then 
                    call read_stderr(pg, errmsg)
                    print *, errmsg
                end if
            end block
            
            prev = idx + len(extension)
            idx = index(files(prev:), extension) + prev - 1
            if (idx >= len(files) - len(extension)) exit
        end do
    END_TEST

    TEST(test_hello_world)
        use subprocess, only: process, run
        use test_subs

        type(process) :: p

#ifndef _FPM
        character(*), parameter :: dirpath = 'TestData/'
#else
        character(*), parameter :: dirpath = 'tests/TestData/'
#endif
    
        p = process(dirpath//'hello_world.exe', stdout=write_stdout)
        call run(p)
    
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_STREQ(output, 'Hello from child!')
    END_TEST
!
!    TEST(test_process_return_argc)
!        use subprocess, only: process, run, read_stdout
!        use test_subs
!
!        type(process) :: p1, p2
!
!        character(:), allocatable :: argc
!#ifndef _FPM
!        character(*), parameter :: dirpath = 'TestData/'
!        character(*), parameter :: incpath = '../include'
!#else
!        character(*), parameter :: dirpath = 'tests/TestData/'
!        character(*), parameter :: incpath = 'include'
!#endif
!        p1 = process('gfortran')
!        call run(p1, dirpath//'process_return_argc.f90', & 
!                        '-o '//dirpath//'process_return_argc', &
!                        '-cpp -I'//incpath)
!
!        EXPECT_TRUE(p1%exit_code() == 0)
!    
!        p2 = process(dirpath//'process_return_argc.exe')
!        call run(p2, '--test')
!    
!        EXPECT_TRUE(p2%exit_code() == 0)
!        call read_stdout(p2, argc)
!        EXPECT_STREQ(trim(argc), '1')
!    END_TEST

!     TEST(process_stdout_large)
!         use subprocess, only: process, run, read_stdout
!         use test_subs

!         type(process) :: p1, p2

!         character(:), allocatable :: stdout
! #ifndef _FPM
!         character(*), parameter :: dirpath = 'TestData/'
!         character(*), parameter :: incpath = '../include'
! #else
!         character(*), parameter :: dirpath = 'tests/TestData/'
!         character(*), parameter :: incpath = 'include'
! #endif
!         p1 = process('gfortran')
!         call run(p1, dirpath//'process_stdout_large.f90', & 
!                         '-o '//dirpath//'process_stdout_large', &
!                         '-cpp -I'//incpath)

!         EXPECT_TRUE(p1%exit_code() == 0)
    
!         p2 = process(dirpath//'process_stdout_large.exe')
!         call p2%run('5')
    
!         EXPECT_TRUE(p2%exit_code() == 0)
!         call p2%read_stdout(stdout)
!         EXPECT_STREQ(trim(stdout), '1')
!     END_TEST

!    TEST(process_stdout_poll)
!        use subprocess, only: process, run, read_stdout
!        use test_subs
!
!        type(process) :: p1, p2
!        procedure(process_io), pointer :: winput
!
!        character(:), allocatable :: data
!#ifndef _FPM
!        character(*), parameter :: dirpath = 'TestData/'
!        character(*), parameter :: incpath = '../include'
!#else
!        character(*), parameter :: dirpath = 'tests/TestData/'
!        character(*), parameter :: incpath = 'include'
!#endif
!        p1 = process('gfortran')
!        call run(p1, dirpath//'process_stdout_poll.f90', & 
!                        '-o '//dirpath//'process_stdout_poll', &
!                        '-cpp -I'//incpath)
!
!        EXPECT_TRUE(p1%exit_code() == 0)
!
!        p2 = process(dirpath//'process_stdout_poll.exe', winput)
!        call p2%runasync('1')
!
!        do while(.not. allocated(data))
!            call p2%read_stdout(data)
!            if (allocated(data)) then
!                call winput(p2, 's')
!            end if
!        end do
!
!        call p2%wait()
!    END_TEST

END_TESTPROGRAM