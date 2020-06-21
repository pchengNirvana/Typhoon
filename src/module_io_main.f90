module module_io_main
  implicit none

contains

  subroutine write_r17 &
    (directory, filename, nt, r17)
    implicit none

    character(len = *), intent (in) :: directory, filename
    integer, intent (in) :: nt
    real(4), intent (in) :: r17(nt)

    integer :: lun
    integer :: t

    ! open output file
    open(lun, file = directory//'/'//filename, status = 'replace')

    ! write r17 to output file
    do t = 1, nt
      write(lun, *) r17(t)
    end do

    ! close output file
    close(lun)
  end subroutine write_r17

end module module_io_main
