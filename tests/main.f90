module test_subs
    use subprocess, only: process, process_io
    
    implicit none; private
    
    public :: write_stdout,     &
              output, stdin,    &
              dirpath, incpath, &
              setvar

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
        class(process), intent(in)  :: sender
        character(*), intent(in)    :: msg
    
        output = trim(msg)
    end subroutine

    subroutine setvar(name, value, ierr)
        use iso_c_binding
        character(*), intent(in)        :: name
        character(*), intent(in)        :: value
        integer, intent(out), optional  :: ierr

        interface
#ifdef _WIN32
        integer(c_int) function putenv_c(name, value) bind(c, name='_putenv_s')
            import
            implicit none
            character(kind=c_char, len=1), intent(in) :: name(*)
            character(kind=c_char, len=1), intent(in) :: value(*)
        end function
#else
        integer(c_int) function setenv_c(name, value, overwrite) bind(c, name='setenv')
            import
            implicit none
            character(kind=c_char, len=1), intent(in) :: name(*)
            character(kind=c_char, len=1), intent(in) :: value(*)
            integer(c_int), value :: overwrite
        end function
#endif
    end interface

        !private
        integer(c_int) :: res, overwrite
        overwrite = 1
#ifdef _WIN32
        res = putenv_c(name//c_null_char, value//c_null_char)
#else
        res = setenv_c(name//c_null_char, value//c_null_char, overwrite)
#endif
        if (present(ierr)) ierr = res
    end subroutine
end module
    
#include <assertion.inc>
TESTPROGRAM(main)

    TEST(test_gfortran)
        use subprocess, only: process, run, read_stderr, read_stdout
        use test_subs

        type(process) :: p1
        character(:), allocatable :: files
        character(:), allocatable :: file
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
                logical :: exists
#ifdef COMPILE_EXAMPLES
                exists = .false.
#else
                inquire(file=dirpath//file//'.exe', exist=exists)
#endif
                if (.not. exists) then
                    pg = process('gfortran')
                    !the static options are necessary to test the program without passing the 
                    !parent environmental variables (especially the PATH variable)
                    call run(pg, dirpath//file//extension, '-o '//dirpath//file, '-cpp -I'//incpath, & 
                             '-static-libgcc -Wl,-Bstatic,--whole-archive -lwinpthread -Wl,--no-whole-archive')
                    code = pg%exit_code()
                    EXPECT_TRUE(code == 0)
                    if (code /= 0) then 
                        call read_stderr(pg, errmsg)
                        print *, errmsg
                    end if
                else
                    EXPECT_TRUE(exists)
                end if
            end block
            
            prev = idx + len(extension)
            idx = index(files(prev:), extension) + prev - 1
            if (idx >= len(files) - len(extension)) exit
        end do
    END_TEST

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
        EXPECT_EQ(res, '40')
    END_TEST
    
    TEST(subprocess_ping)
        use subprocess
        use test_subs

        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: commandline = 'ping'
        integer :: i

        p = process(commandline)
        call runasync(p, '-t', 'www.bbc.co.uk')

        res = 'a'
        i = 0
        do while (len_trim(res) /= 0)
            i = i + 1
            call read_stdout(p, res)
            if (i > 10) exit
        end do
        call kill(p)
        EXPECT_EQ(i, 11)
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

        p = process(dirpath//commandline)
        call run(p, 'noquotes', '"""should be quoted"""')

        EXPECT_TRUE(p%exit_code() == 0)
        call read_stdout(p, res)

        EXPECT_STREQ(res, compare)
    END_TEST

    TEST(process_combined_stdout_stderr)
        use subprocess
        use test_subs

        character(*), parameter :: commandline = 'process_combined_stdout_stderr'
        type(process) :: p
        character(:), allocatable :: res
        character(*), parameter :: compare = "Hello,It's me!world!Yay!"

        p = process(dirpath//commandline)
        call run(p, option_combined_stdout_stderr)

        EXPECT_TRUE(p%exit_code() == 0)

        call read_stdout(p, res)

        EXPECT_STREQ(res, compare);
    END_TEST

    TEST(process_not_inherit_environment)
        use subprocess
        use test_subs

        character(*), parameter :: commandline = 'process_inherit_environment'
        type(process) :: p
        character(:), allocatable :: res
        integer :: ierr

        call setvar('PROCESS_ENV_TEST', '1', ierr)
        EXPECT_EQ(ierr, 0)
        p = process(dirpath//commandline)

        call run(p, option_none)
        call read_stdout(p, res)
        EXPECT_STREQ(res, '0')

        EXPECT_TRUE(p%exit_code() == 0)
    END_TEST

    TEST(process_inherit_environment)
        use subprocess
        use test_subs

        character(*), parameter :: commandline = 'process_inherit_environment'
        type(process) :: p
        integer :: ierr
        character(:), allocatable :: res
        
        call setvar('PROCESS_ENV_TEST', '42', ierr)
        EXPECT_EQ(ierr, 0)
        p = process(dirpath//commandline)
        
        call run(p, option_inherit_environment)
        call read_stdout(p, res)
        EXPECT_STREQ(res, '42')
        
        EXPECT_TRUE(p%exit_code() == 0)
    END_TEST

    TEST(process_fail_divzero)
        use subprocess
        use test_subs

        character(*), parameter :: commandline = 'process_fail_divzero'
        type(process) :: p

        p = process(dirpath//commandline)
        call run(p)

        ! On AArch64 systems divide by zero does not cause a failure.
#if !((defined(__arm64__) && defined(__APPLE__)) || defined(__aarch64__))
        EXPECT_NE(p%exit_code(), 0)
#endif
    END_TEST

END_TESTPROGRAM