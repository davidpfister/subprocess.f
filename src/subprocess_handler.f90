module subprocess_handler
    use, intrinsic :: iso_c_binding

    implicit none; private

    public  :: process_start
    public  :: process_close
    public  :: process_readline
    public  :: process_kill

    logical(c_bool), parameter :: logical_false = .false.
    logical(c_bool), parameter :: logical_true = .true.

    type, public       :: handle_pointer
        type(c_ptr)    :: handle = c_null_ptr
    end type

    interface
#ifndef _WIN32
        function popen_c(command, mode) &
            bind(C, name='popen')
            use, intrinsic :: iso_c_binding
            character(c_char), dimension(*) :: command, mode
            type(c_ptr) :: popen_c
        end function
#else
        function popen_c(command, mode, pid) &
            bind(C, name='winpopen')
            use, intrinsic :: iso_c_binding
            character(c_char), dimension(*) :: command, mode
            integer(c_int), intent(out) :: pid
            type(c_ptr) :: popen_c
        end function
#endif

        function fgets_c(buf, siz, handle) &
#ifndef _WIN32
            bind(C, name='fgets')
#else
            bind(C, name='winfgets')
#endif
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: fgets_c
            character(c_char), intent(inout) :: buf(*)
            integer(c_int), intent(out) :: siz
            type(c_ptr), value :: handle
        end function

        function pclose_c(handle) &
#ifndef _WIN32
            bind(C, name='pclose')
#else
            bind(C, name='winpclose')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int) :: pclose_c
            type(c_ptr), value :: handle
        end function

        function fputs_c(buf, handle) &
#ifndef _WIN32
            bind(C, name='fputs')
#else
            bind(C, name='fputs')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int) :: fputs_c
            character(c_char), dimension(*) :: buf
            type(c_ptr), value :: handle
        end function

        function fflush_c(handle) &
#ifndef _WIN32
            bind(C, name='fflush')
#else
            bind(C, name='winfflush')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int) :: fflush_c
            type(c_ptr), value :: handle
        end function

        function pipe_c(fd) &
#ifndef _WIN32
            bind(C, name='pipe')
#else
            bind(C, name='_pipe')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int), intent(in) :: fd(2)
            integer(c_int)             :: pipe_c
        end function


        function fork_c() & 
#ifndef _WIN32
            bind(c, name='fork')
            use, intrinsic :: iso_c_binding
            integer(c_int32_t) :: fork_c
#else
            bind(C, name='winfork')
            use, intrinsic :: iso_c_binding
            integer(c_intptr_t) :: fork_c
#endif     
        end function

#ifndef _WIN32
        function setpgid_c(pid, pgid) bind(C, name='setpgid')
            use, intrinsic :: iso_c_binding
            integer(c_int32_t) :: pid, pgid
            integer(c_int) :: setpgid_c
        end function
#endif

        function close_c(fd) &
#ifndef _WIN32
            bind(C, name='close')
#else
            bind(C, name='close')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int), intent(in), value :: fd
            integer(c_int)                    :: close_c
        end function

        function dup2_c(old_fd, new_fd) &
#ifndef _WIN32
            bind(C, name='dup2')
#else
            bind(C, name='dup2')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int), intent(in), value :: old_fd
            integer(c_int), intent(in), value :: new_fd
            integer(c_int)                    :: dup2_c
        end function

        function execl_c(path, arg1, arg2) &
#ifndef _WIN32
            bind(C, name='execl')
#else
            bind(C, name='execl')
#endif
            use, intrinsic :: iso_c_binding
            character(c_char), intent(in)        :: path
            character(c_char), intent(in)        :: arg1
            character(c_char), intent(in)        :: arg2
            integer(c_int)                       :: execl_c
        end function

        function fdopen_c(fd, mode) &
#ifndef _WIN32
            bind(C, name='fdopen')
#else
            bind(C, name='fdopen')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int), intent(in), value :: fd
            character(c_char), intent(in)        :: mode
            type(c_ptr)                          :: fdopen_c
        end function

        function kill_c(pid, sig) &
#ifndef _WIN32
            bind(C, name='kill')
#else
            bind(C, name='winkill')
#endif
            use, intrinsic :: iso_c_binding
            integer(c_int32_t), intent(in), value :: pid
            integer(c_int), intent(in), value   :: sig
            integer(c_int)                        :: kill_c
        end function
    end interface

contains

    !> @brief Execute a child program, determine whether it succeeded or failed based
    !! on the last line of text that the program wrote to its standard output.
    !!
    !! @param[in]     cmd     The command line to execute.  Typically
    !! the first argument would be the same as @a name.
    !!
    !! @param[out]    success_flag      Flag to indicate whether successful
    !! termination of the cmd was detected.  This is based on the last
    !! line of text that was written by the program - if it is the same as
    !! success_text then it is assumed that the program succeeded.
    integer function process_start(cmd, mode, fp, ierr) result(pid)
        character(*), intent(in)                :: cmd
        character(*), intent(in)                :: mode
        type(handle_pointer), intent(out)       :: fp
        integer, intent(out), optional          :: ierr
        !private
        integer                                 :: current_pid
        integer :: istat

        istat = 0
        if (present(ierr)) ierr = istat

#ifndef _WIN32
        fp%handle = popen_f(cmd, mode, current_pid, istat)
        pid = current_pid
#else
        fp%handle = popen_c(trim(cmd)//c_null_char, trim(mode)//c_null_char, current_pid)
        pid = current_pid
#endif

        if (.not. c_associated(fp%handle)) then
            write (*, *) '*process_start* ERROR: Could not open pipe!'
            istat = -1
        end if
        if (present(ierr)) ierr = istat
    end function

    function popen_f(cmd, mode, pid, ierr)
        integer, intent(out)             :: ierr !< status for attempt to open process (0= no error)
        character(*), intent(in)         :: cmd !< shell command to start process with
        character(*), intent(in)         :: mode !< read/write/mode parameter to pass to popen(3c)
        type(c_ptr)                      :: popen_f
        integer(c_int32_t), intent(out)  :: pid
        integer(c_int32_t)               :: child_pid = 0
        integer                          :: fd(2)
        integer                          :: rc ! Return code.

        rc = pipe_c(fd)
#ifndef _WIN32
        child_pid = fork_c()
#endif
        if (child_pid == -1) then
            ierr = 1
            return
        end if

        if (child_pid .eq. 0) then
            if (mode == 'r') then
                rc = close_c(fd(1)) ! Close the READ end of the pipe since the child's fd is write-only
                rc = dup2_c(fd(2), 1) ! Redirect stdout to pipe
            else
                rc = close_c(fd(2)) ! Close the WRITE end of the pipe since the child's fd is read-only
                rc = dup2_c(fd(1), 0) ! Redirect stdin to pipe
            end if
#ifndef _WIN32
            rc = setpgid_c(child_pid, child_pid) !Needed so negative PIDs can kill children of /bin/sh
#endif
            rc = execl_c(trim(cmd)//c_null_char, trim(cmd)//c_null_char, c_null_char); 
            ierr = rc
            return
        else
            if (mode == 'r') then
                rc = close_c(fd(2)) !Close the WRITE end of the pipe since parent's fd is read-only
            else
                rc = close_c(fd(1)) !Close the READ end of the pipe since parent's fd is write-only
            end if
        end if

        pid = child_pid

        if (mode == 'r') then
            popen_f = fdopen_c(fd(1), "r"//c_null_char)
        else
            popen_f = fdopen_c(fd(2), "w"//c_null_char)
        end if

    end function

    subroutine process_close(fp, ierr)
        integer, intent(out)                  :: ierr
        type(handle_pointer), intent(inout)   :: fp
        integer(c_int)                        :: ios
        ios = 0

        if (.not. c_associated(fp%handle)) then
            write (*, *) '*process_close* process not found'
        else
#ifndef _WIN32
            ios = fflush_c(fp%handle)
#else
            ios = fflush_c(fp%handle)
#endif
            if (ios >= 0) then
#ifndef _WIN32
                ios = pclose_c(fp%handle)
#else
                ios = pclose_c(fp%handle)
#endif
            end if
        end if
        ierr = ios
    end subroutine

    !> @brief Read a line from a windows handle.
    !! @param[in]     fp       The handle to read from.
    !! @param[out]    readfrom              The line read from the handle.
    !! @param[out]    ierr              Error ierr
    !! A line is terminated by either a line feed (ACHAR(10)) or a
    !! carriage return/line feed combination (ACHAR(13) // ACHAR(10)).
    subroutine process_readline(readfrom, fp, ierr)
        character(:), allocatable, intent(out)  :: readfrom ! readfrom length must be at least two
        type(handle_pointer), intent(in)        :: fp
        integer, intent(out)                    :: ierr
        !private
        character(4095)                         :: buf
        integer(c_int)                          :: clen
        integer                                 :: eos, i
        integer                                 :: ios
        if (allocated(readfrom)) then
            clen = len(readfrom) - 1
        else
            clen = -1
        end if

        readfrom = ' '

        do while (c_associated(fgets_c(buf, clen, fp%handle)))
            eos = 2
            do i = 1, clen + 1
                if (buf(i:i) == c_null_char) then
                    eos = i - 2 ! assuming line terminator character
                    if (eos > -1) then
                        buf(eos + 1:) = ' '
                        readfrom = trim(buf)
                        exit
                    end if
                end if
            end do
            ios = 0
            return
        end do
        ! an error occurred
        ios = 0
        ierr = min(-1, ios)
    end subroutine

    subroutine process_kill(pid)
        integer, parameter               :: SIGKILL = 4
        integer, intent(in)         :: pid
        integer                     :: rc

        if (pid > 0) then
            rc = kill_c(pid, SIGKILL)
        end if
    end subroutine
end module
