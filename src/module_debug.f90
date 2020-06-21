module module_debug
  implicit none

contains

  subroutine message_text (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
  end subroutine message_text

  subroutine check_in_file_exists (directory, filename)
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

  subroutine check_in_directory_exists (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'ERROR: Input file directory does not exist: '//directory
      stop
    end if
  end subroutine check_in_directory_exists

  subroutine check_out_directory_exists (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'Creating output directory: '//directory
      call system('mkdir -p '//directory)
    end if
  end subroutine check_out_directory_exists

  subroutine debug_in_file (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Input file: '//directory//'/'//filename
  end subroutine debug_in_file

  subroutine debug_out_file (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Output file: '//directory//'/'//filename
  end subroutine debug_out_file

  subroutine error_message_text (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
    stop
  end subroutine error_message_text

  subroutine error_message_env_not_set (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') 'ERROR: Variable is not set in the environment: '//text
    stop
  end subroutine error_message_env_not_set

  subroutine error_message_var_not_numeric (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') 'ERROR: Variable is not a number: '//text
    stop
  end subroutine error_message_var_not_numeric

  subroutine error_message_domain_number_exceed (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') 'ERROR: Output domain number exceeds 99: '//text
    stop
  end subroutine error_message_domain_number_exceed

  subroutine error_message_unknown (text, text2)
    implicit none

    character(len = *), intent (in) :: text, text2

    write(*, '(a)') 'ERROR: Unknown variable setting: '//text//' = '//text2
    stop
  end subroutine error_message_unknown

end module module_debug
