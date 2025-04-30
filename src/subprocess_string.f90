!> @defgroup group_subprocess_string String
!! @ingroup group_api
!! @brief @link subprocess_string::string String @endlink module
module subprocess_string
    implicit none; private

    public :: trim

    !> @class string 
    !! @ingroup group_subprocess_string
    !! @brief A derived type representing a string with assignment functionality.
    !! This type provides a simple wrapper around a character string with a custom assignment
    !! operator and a trimming function.
    !! <h2>Examples</h2>
    !! ```fortran
    !! type(string) :: s
    !! s = 'foo'
    !! ```
    !! <h2>Remarks</h2>
    !! @par
    !! The string implementation proposed here is kept at the bare 
    !! minimum of what is required by the library. There are many 
    !! other implementations that can be found.
    !! <h2>Constructors</h2>
    !! Initializes a new instance of the @ref string class
    !! <h3>string(character(:))</h3>
    !! @verbatim type(string) function string(character(:) chars) @endverbatim
    !! 
    !! @param[in] chars 
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! s = string('foo')
    !! ```
    type, public :: string
        private
        character(:), allocatable, public :: chars !< Variable length character array
    contains
        procedure, private, pass(lhs) :: string_assign_character
        generic :: assignment(=)      => string_assign_character
    end type

    !> @interface trim
    !! @ingroup group_subprocess_string
    !> @brief Trims whitespace from a string object.
    !! @par
    !! <h2>Methods</h2>
    !!
    !! <h3>trim(type(string) str)</h3>
    !! 
    !! @param[in] str The string object to trim.
    !! @return res The trimmed character string.
    !! 
    !! <h2> Examples </h2>
    !! The following example illustrates a call to the `trim` interface.
    !! @code{.f90}
    !!  character(:), allocatable :: res
    !!  type(string) :: str
    !!  str = 'Hello          '
    !!  res = trim(str)
    !!
    !! print*, res
    !! !Should print 'Hello'
    !! @endcode
    !! <h2> Remarks </h2>
    interface trim
    !! @cond
        module procedure :: string_trim
    !! @endcond
    end interface

    contains

    !! @ingroup group_subprocess_string
    !> @brief Assigns a character string to a string object.
    !! @param[in,out] lhs The string object to assign to.
    !! @param[in] rhs The character string to assign.
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! character(:), allocatable :: c
    !! 
    !! s = 'foo'
    !! c = s
    !! ! The value of c is now 'foo'
    !! ```
    !! @b Remarks
    pure subroutine string_assign_character(lhs, rhs)
        class(string), intent(inout) :: lhs
        character(*), intent(in)     :: rhs

        lhs%chars = rhs
    end subroutine

    function string_trim(str) result(res)
        class(string), intent(in) :: str
        character(:), allocatable :: res
        
        res = trim(adjustl(str%chars))
    end function
end module