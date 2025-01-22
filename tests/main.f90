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
        character(*), parameter :: dirparth = "TestData/"
#else
        character(*), parameter :: dirparth = "tests/TestData/"
#endif
        p = process("gfortran")
        call p%with_arg(dirparth//"hello_world.f90 -o "//dirparth//"hello_world") ! contains "print *, "Hello from child!"; end
        call p%run()

        EXPECT_TRUE(p%exit_code() == 0)
        p = process(dirparth//"hello_world.exe", stdout=write_stdout)
        call p%run()
        
        EXPECT_TRUE(p%exit_code() == 0)
    END_TEST

END_TESTPROGRAM