module subprocess_sleep
    use, intrinsic :: iso_c_binding, only: c_int

    implicit none

    private

    public :: sleep

    interface
        subroutine sleep_c(millseconds) bind(c, name='c_sleep')
            import :: c_int
            !DEC$ ATTRIBUTES STDCALL :: sleep_c
            !GCC$ ATTRIBUTES STDCALL :: sleep_c
            integer(c_int), intent(in), value :: millseconds
        end subroutine
    end interface
contains

    subroutine sleep(millisec)
        integer, intent(in), optional :: millisec
        integer(c_int) :: ierr
        !private
        integer :: sleeptime
        sleeptime = 100
        if (present(millisec)) sleeptime = millisec

        call sleep_c(int(millisec, c_int))
    end subroutine
end module