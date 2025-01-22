module test_subs
    implicit none
    
    contains
    
    subroutine write_stdout(sender, msg)
        class(*), intent(in) :: sender
        character(*), intent(in) :: msg
    
        print*, msg
    end subroutine
end module
    
#include <assertion.inc>
TESTPROGRAM(main)

    TEST(test_gfortran)
        use subprocess, only: process
        use test_subs
        
        type(process) :: p

#ifndef _FPM
        character(*), parameter :: dirpath = "TestData/"
#else
        character(*), parameter :: dirpath = "tests/TestData/"
#endif
        p = process("gfortran")
        call p%with_arg(dirpath//"hello_world.f90 -o "//dirpath//"hello_world") ! contains "print *, "Hello from child!"; end
        call p%run()

        EXPECT_TRUE(p%exit_code() == 0)
        p = process(dirpath//"hello_world.exe", stdout=write_stdout)
        call p%run()
        
        EXPECT_TRUE(p%exit_code() == 0)
    END_TEST

    TEST(test_process_return_argc)
        use subprocess, only: process
        use test_subs
        
        type(process) :: p

#ifndef _FPM
        character(*), parameter :: dirpath = "TestData/"
        character(*), parameter :: incpath = "../include"
#else
        character(*), parameter :: dirpath = "tests/TestData/"
        character(*), parameter :: incpath = "include"
#endif
        p = process("gfortran")
        call p%with_arg(dirpath//"process_return_argc.f90", & 
                        "-o "//dirpath//"process_return_argc", &
                        "-cpp -I"//incpath) ! contains "print *, "Hello from child!"; end
        call p%run()

        EXPECT_TRUE(p%exit_code() == 0)
        p = process(dirpath//"process_return_argc.exe")
        call p%run()
        
        EXPECT_TRUE(p%exit_code() == 0)
    END_TEST

END_TESTPROGRAM