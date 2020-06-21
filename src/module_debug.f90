module module_debug
  ! Written by P. Cheng
  implicit none

contains

  subroutine message_text &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
  end subroutine message_text

  subroutine debug_dimension_settings &
    (nx, ny, nz, nt, nr)
    implicit none

    integer, intent (in) :: nx, ny, nz, nt, nr

    write(*, '(a)') 'DEBUG: Current dimension setting is: '
    write(*, '(a, i4)') '       nx = ', nx
    write(*, '(a, i4)') '       ny = ', ny
    write(*, '(a, i4)') '       nz = ', nz
    write(*, '(a, i4)') '       nt = ', nt
    write(*, '(a, i4)') '       nr = ', nr
  end subroutine debug_dimension_settings

  subroutine check_in_file_exists &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    logical :: exists

    inquire(file = directory//'/'//filename, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'ERROR: Input file does not exist: '//&
        directory//'/'//filename
      stop
    end if
  end subroutine check_in_file_exists

  subroutine check_in_directory_exists &
    (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'ERROR: Input file directory does not exist: '//directory
      stop
    end if
  end subroutine check_in_directory_exists

  subroutine debug_in_file &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Input file: '//directory//'/'//filename
  end subroutine debug_in_file

  subroutine debug_check_read_variables &
    (nx, ny, nz, nt, u10, v10, slp)
    implicit none

    integer, intent (in) :: nx, ny, nz, nt
    real(4), intent (in), optional :: u10(nx, ny, nt)
    real(4), intent (in), optional :: v10(nx, ny, nt)
    real(4), intent (in), optional :: slp(nx, ny, nt)

  end subroutine debug_check_read_variables

  subroutine check_out_directory_exists &
    (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'Creating output directory: '//directory
      call system('mkdir -p '//directory)
    end if
  end subroutine check_out_directory_exists

  subroutine debug_out_file &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Output file: '//directory//'/'//filename
  end subroutine debug_out_file

  subroutine error_message_text &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
    stop
  end subroutine error_message_text

  subroutine error_message_env_not_set &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') 'ERROR: Environment variable is not set: '//text
    stop
  end subroutine error_message_env_not_set

  subroutine error_message_env_not_number &
    (text, text2)
    implicit none

    character(len = *), intent (in) :: text, text2

    write(*, '(a)') 'ERROR: Environment variable is not a number: '//&
      text//' = '//text2
    stop
  end subroutine error_message_env_not_number

  subroutine error_message_env_unknown &
    (text, text2)
    implicit none

    character(len = *), intent (in) :: text, text2

    write(*, '(a)') 'ERROR: Unknown environment variable setting: '// &
      text//' = '//text2
    stop
  end subroutine error_message_env_unknown

end module module_debug
