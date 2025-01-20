#include <assertion.inc>
TESTPROGRAM(main)

    TEST(test_gfortran)
        use subprocess, only: process

        type(process) :: p
        logical :: succ
        integer :: code

#ifndef _FPM
        character(*), parameter :: dirparth = "TestData/"
#else
        character(*), parameter :: dirparth = "tests/TestData/"
#endif
        p = process("gfortran")
        call p%with_arg(dirparth//"hello_world.f90 -o "//dirparth//"hello_world") ! contains "print *, "Hello from child!"; end
        call p%run(success=succ, code=code)

        EXPECT_TRUE(succ)
        p = process(dirparth//"hello_world.exe")
        call p%run(success=succ, code=code)
        
        EXPECT_TRUE(succ)
	    EXPECT_STREQ(p%output(:18), ' Hello from child!')
    END_TEST

END_TESTPROGRAM