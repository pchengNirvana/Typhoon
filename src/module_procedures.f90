module module_procedures
  implicit none

contains

  function is_numeric(string)
    implicit none

    character(len = *), intent (in) :: string
    character(len = :), allocatable :: newstring
    logical :: is_numeric
    real(4) :: x
    integer :: iostats, stringlength

    stringlength = len(trim(adjustl(string)))
    allocate(character(len = stringlength) :: newstring)
    newstring = trim(adjustl(string))
    read(newstring, *, iostat = iostats) x
    is_numeric = iostats == 0
    if (allocated(newstring)) deallocate(newstring)
  end function is_numeric

end module module_procedures
