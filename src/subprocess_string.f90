!> @file
!! @defgroup group_subprocess_string String
!! Simple string wrapper type with assignment operator and trimming support.
!!
!! @b Features
!! - Lightweight derived type wrapping allocatable character string
!! - Overloaded assignment operator from character to string type
!! - Generic trim interface that works with the string type
!! - Minimal implementation focused on the needs of the subprocess library
!! - Pure procedures for safe use in performance-critical code
!!
!! <h2 class="groupheader">Examples</h2>
!! Basic usage:
!! @code{.f90}
!! use subprocess_string
!!
!! type(string) :: s
!! character(:), allocatable :: text
!!
!! s = 'Hello World   '     ! Uses assignment operator
!! text = trim(s)           ! Uses trim interface
!!
!! print *, 'Trimmed: ', text
!! ...
!! @endcode
!!
!! Constructor-style initialization:
!! @code{.f90}
!! use subprocess_string
!!
!! type(string) :: s
!! s = string('   Fortran is great   ')
!! print *, trim(s)   ! prints 'Fortran is great'
!! ...
!! @endcode
module subprocess_string
    implicit none; private

    public :: trim

    !> A derived type representing a string with assignment functionality.
    !! This type provides a simple wrapper around a character string with a custom assignment
    !! operator and a trimming function.
    !! <h2  class="groupheader">Examples</h2>
    !! ```fortran
    !! type(string) :: s
    !! s = 'foo'
    !! ```
    !! @note
    !! The string implementation proposed here is kept at the bare 
    !! minimum of what is required by the library. There are many 
    !! other implementations that can be found.
    !! @par
    !! <h2  class="groupheader">Constructors</h2>
    !! Initializes a new instance of the @ref string class
    !! <h3>string(character(:))</h3>
    !! @verbatim type(string) function string(character(:) chars) @endverbatim
    !! 
    !! @param[in] chars The character array
    !! 
    !! @b Examples
    !! ```fortran
    !! type(string) :: s
    !! s = string('foo')
    !! ```
    !! @par
    !! <h2  class="groupheader">Remarks</h2>
    !! @ingroup group_subprocess_string
    type, public :: string
        private
        character(:), allocatable, public :: chars !< Variable length character array
    contains
        procedure, private, pass(lhs) :: string_assign_character
        procedure, private, pass(rhs) :: character_assign_string
        generic :: assignment(=)      => string_assign_character, &
                                         character_assign_string
    end type

    !> Trims whitespace from a string object. 
    !! <h2  class="groupheader">Methods</h2>
    !!
    !! <h3>trim(type(string) str)</h3>
    !! 
    !! @param[in] str The string object to trim.
    !! @return res The trimmed character string.
    !! 
    !! <h2  class="groupheader">Examples</h2>
    !! The following example illustrates a call to the `trim` interface.
    !! @code{.f90}
    !!  character(:), allocatable :: res
    !!  type(string) :: str
    !!  str = 'Hello          '
    !!  res = trim(str)
    !!
    !! print*, res
    !! !Should print 'Hello'
    !! ...
    !! @endcode
    !! <h2> Remarks </h2>
    !! @ingroup group_subprocess_string
    interface trim
    !! @cond
        module procedure :: string_trim
    !! @endcond
    end interface

    contains

    !> Assigns a character string to a string object.
    !! @param[in,out] lhs The string object to assign to.
    !! @param[in] rhs The character string to assign.
    !! 
    !! @b Examples
    !! @code{.f90}
    !! type(string) :: s
    !! character(:), allocatable :: c
    !! 
    !! s = 'foo'
    !! c = s
    !! ! The value of c is now 'foo'
    !! ...
    !! @endcode
    !! @ingroup group_subprocess_string
    !! @b Remarks
    pure subroutine string_assign_character(lhs, rhs)
        class(string), intent(inout) :: lhs
        character(*), intent(in)     :: rhs

        lhs%chars = rhs
    end subroutine
    
    !> Assigns a string object to a character string.
    !! @param[in,out] lhs The character string to assign to.
    !! @param[in] rhs The string object to assign.
    !! 
    !! @b Examples
    !! @code{.f90}
    !! character(:), allocatable :: c
    !! type(string) :: s
    !! 
    !! s = string('foo')
    !! c = s
    !! ! The value of c is now 'foo'
    !! ...
    !! @endcode
    !! @ingroup group_subprocess_string
    !! @b Remarks
    pure subroutine character_assign_string(lhs, rhs)
        character(:), allocatable, intent(inout)    :: lhs
        class(string), intent(in)                   :: rhs   

        lhs = rhs%chars
    end subroutine

    function string_trim(str) result(res)
        class(string), intent(in) :: str
        character(:), allocatable :: res
        
        res = trim(adjustl(str%chars))
    end function
end module