module test_subs
    use subprocess, only: process, process_io
    
    implicit none; private
    
    public :: write_stdout,     &
              output, stdin,    &
              dirpath, incpath

#ifndef _FPM
    character(*), parameter :: dirpath = 'TestData/'
    character(*), parameter :: incpath = '../include'
#else
    character(*), parameter :: dirpath = 'tests/TestData/'
    character(*), parameter :: incpath = 'include'
#endif
    
    character(:), allocatable :: output
    procedure(process_io), pointer :: stdin => null()
    
    contains
    
    subroutine write_stdout(sender, msg)
        type(process), intent(in)   :: sender
        character(*), intent(in)    :: msg
    
        output = trim(msg)
    end subroutine
end module
    
#include <assertion.inc>
TESTPROGRAM(main)

#ifdef COMPILE_EXAMPLES
    TEST(test_gfortran)
        use subprocess, only: process, run, read_stderr, read_stdout
        use test_subs

        type(process) :: p1
        character(:), allocatable :: files, file
        character(*), parameter :: extension = '.f90'
        integer :: idx, code, space
        character(:), allocatable :: errmsg
        integer :: prev

#ifdef _WIN32
        p1 = process('cmd')
        call run(p1, '/c dir "'//dirpath//'" *'//extension)
#else
        p1 = process('ls')
        call run(p1, dirpath//' *'//extension)
#endif
       
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
#endif

    TEST(test_hello_world)
        use subprocess
        use test_subs

        type(process) :: p
    
        p = process(dirpath//'hello_world', stdout=write_stdout)
        call run(p)
    
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_STREQ(output, 'Hello from child!')
    END_TEST
    
    TEST(process_return_zero)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_return_zero'

        p = process(dirpath//commandline)
        call runasync(p, '0')
        EXPECT_TRUE(p%pid /= 0)
        call wait(p)
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_TRUE(p%has_exited())
    END_TEST
    
    TEST(subprocess_return_fortytwo)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'process_return_fortytwo'

        p = process(dirpath//commandline)
        call run(p)
        call read_stdout(p, res)
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_STREQ(res, '42')
    END_TEST
    
    TEST(subprocess_return_argc)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'process_return_argc'

        p = process(dirpath//commandline)
        call run(p, 'foo', 'bar', 'baz', 'faz')
        call read_stdout(p, res)
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_STREQ(res, '4')
    END_TEST
    
    TEST(subprocess_return_argv)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'process_return_argv'

        p = process(dirpath//commandline)
        call run(p, char(10)//char(13)//char(10)//'13')
        call read_stdout(p, res)
        EXPECT_TRUE(p%exit_code() == 0)
        EXPECT_STREQ(res, '13')
    END_TEST
    
    TEST(subprocess_return_stdin)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_return_stdin'

        p = process(dirpath//commandline, stdin=stdin)
        call runasync(p)
        
        call stdin(p, 'a')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'b')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'b')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'a')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, ' ')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'a')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'r')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'e')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, ' ')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'g')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'r')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'e')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 'a')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, 't')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, '!')
        EXPECT_FALSE(p%has_exited())
        call stdin(p, '@')

        call wait(p)
        EXPECT_TRUE(p%has_exited())
    END_TEST
    
    TEST(subprocess_return_stdin_count)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'process_return_stdin_count'
        character(*), parameter :: temp = "Wee, sleekit, cow'rin, tim'rous beastie!"

        p = process(dirpath//commandline, stdin=stdin)
        call runasync(p)

        call stdin(p, temp)
        call wait(p)
        call read_stdout(p, res)
        EXPECT_EQ('40', res);
    END_TEST
    
    TEST(subprocess_stdout_argc)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stdout_argc'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, 'foo', 'bar', 'baz', 'faz')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stdout(p, res)
        EXPECT_STREQ(res, '4')
    END_TEST
    
    TEST(subprocess_stdout_argc_with_empty_strings)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stdout_argc'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, '', '', '', '')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stdout(p, res)
        EXPECT_STREQ(res, '0')
    END_TEST
    
    TEST(subprocess_stdout_argv)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stdout_argv'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, 'foo', 'bar', 'baz', 'faz')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stdout(p, res)
        EXPECT_STREQ(res, 'foo bar baz faz')
    END_TEST
    
    TEST(subprocess_stderr_argc)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stderr_argc'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, 'foo', 'bar', 'baz', 'faz')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stderr(p, res)
        EXPECT_STREQ(res, '4')
    END_TEST
    
    TEST(subprocess_stderr_argc_with_empty_strings)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stderr_argc'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, '', '', '', '')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stderr(p, res)
        EXPECT_STREQ(res, '0')
    END_TEST
    
    TEST(subprocess_stderr_argv)
        use subprocess
        use test_subs

        type(process) :: p
        character(*), parameter :: commandline = 'process_stderr_argv'
        character(:), allocatable :: res

        p = process(dirpath//commandline)
        call run(p, 'foo', 'bar', 'baz', 'faz')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stderr(p, res)
        EXPECT_STREQ(res, 'foo bar baz faz')
    END_TEST

    TEST(process_return_lpcmdline)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'process_return_lpcmdline'
        character(*), parameter :: compare = 'noquotes "should be quoted"'
        integer :: i

        p = process(dirpath//commandline)
        call run(p, 'noquotes', '"""should be quoted"""')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stdout(p, res)

        EXPECT_STREQ(res, compare)
    END_TEST

END_TESTPROGRAM