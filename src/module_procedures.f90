module module_procedures
  implicit none

contains

  function is_numeric(string)
    implicit none

    character(len = *), intent (in) :: string
    character(len = 200) :: newstring
    logical :: is_numeric
    real(4) :: x
    integer :: iostats

    newstring = trim(adjustl(string))
    read(trim(newstring), *, iostat = iostats) x
    is_numeric = iostats == 0
  end function is_numeric

end module module_procedures
