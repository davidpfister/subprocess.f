module test_subs
    implicit none; private
    
    public :: write_stdout, &
              output
    
    character(:), allocatable :: output
    
    contains
    
    subroutine write_stdout(sender, msg)
        class(*), intent(in) :: sender
        character(*), intent(in) :: msg
    
        output = trim(msg)
    end subroutine
end module
    
#include <assertion.inc>
TESTPROGRAM(main)

    TEST(test_gfortran)
        use subprocess, only: process, run
        use test_subs

        type(process) :: p1, p2

#ifndef _FPM
        character(*), parameter :: dirpath = 'TestData/'
#else
        character(*), parameter :: dirpath = 'tests/TestData/'
#endif
        p1 = process('gfortran')
        call run(p1, dirpath//'hello_world.f90 -o '//dirpath//'hello_world')

        EXPECT_TRUE(p1%exit_code() == 0)
    
        p2 = process(dirpath//'hello_world.exe', stdout=write_stdout)
        call run(p2)
    
        EXPECT_TRUE(p2%exit_code() == 0)
        EXPECT_STREQ(output, 'Hello from child!')
    END_TEST

    TEST(test_process_return_argc)
        use subprocess, only: process, run, read_stdout
        use test_subs

        type(process) :: p1, p2

        character(:), allocatable :: argc
#ifndef _FPM
        character(*), parameter :: dirpath = 'TestData/'
        character(*), parameter :: incpath = '../include'
#else
        character(*), parameter :: dirpath = 'tests/TestData/'
        character(*), parameter :: incpath = 'include'
#endif
        p1 = process('gfortran')
        call run(p1, dirpath//'process_return_argc.f90', & 
                        '-o '//dirpath//'process_return_argc', &
                        '-cpp -I'//incpath)

        EXPECT_TRUE(p1%exit_code() == 0)
    
        p2 = process(dirpath//'process_return_argc.exe')
        call run(p2, '--test')
    
        EXPECT_TRUE(p2%exit_code() == 0)
        call read_stdout(p2, argc)
        EXPECT_STREQ(trim(argc), '1')
    END_TEST

END_TESTPROGRAM