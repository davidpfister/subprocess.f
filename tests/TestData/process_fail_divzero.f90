#include <app.inc> 
console(process_fail_divzero)
    main(args)
        integer :: p, q, r

        p = 42
        q = return_0_non_optimizable()
        r = p / q ! this is an integer division by zero
        print *, "r=", r
    endmain

    function return_0_non_optimizable() result(res)
      implicit none
      character(len=100) :: buffer
      integer :: value, digit, res
      character(len=1) :: c
      integer :: i

      value = 62831853
      write(buffer, '(I10)') value
      res = 0
      do i = 1, len_trim(buffer)
        c = buffer(i:i)
        digit = ichar(c) - ichar('0')
        res = res + digit
      end do
      res = res - 36
    end function
end