module subprocess_string
    implicit none; private

    public :: trim

    !> @brief A derived type representing a string with assignment functionality.
    !! This type provides a simple wrapper around a character string with a custom assignment
    !! operator and a trimming function.
    type, public :: string
        private
        character(:), allocatable, public :: chars
    contains
        procedure, private, pass(lhs) :: string_assign_character
        generic :: assignment(=)      => string_assign_character
    end type

    interface trim
        module procedure :: string_trim
    end interface

    contains

    !> @brief Assigns a character string to a string object.
    !! @param[in,out] lhs The string object to assign to.
    !! @param[in] rhs The character string to assign.
    pure subroutine string_assign_character(lhs, rhs)
        class(string), intent(inout) :: lhs
        character(*), intent(in)     :: rhs

        lhs%chars = rhs
    end subroutine

    !> @brief Trims whitespace from a string object.
    !! @param[in] str The string object to trim.
    !! @return res The trimmed character string.
    function string_trim(str) result(res)
        class(string), intent(in) :: str
        character(:), allocatable :: res
        
        res = trim(str%chars)
    end function
end module