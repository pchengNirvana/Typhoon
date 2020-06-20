module module_messages
  implicit none

contains

  subroutine message_text (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 1000) text

    1000 format (a)
  end subroutine message_text

  subroutine error_message_text (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2000) text
    stop

    2000 format (a)
  end subroutine error_message_text

  subroutine error_message_env_not_set (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2001) text
    stop
  
    2001 format ('Error: Variable "', a, '" is not set in the environment')
  end subroutine error_message_env_not_set

  subroutine error_message_dir_not_exist (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2002) text
    stop

    2002 format ('Error: Directory "', a, '" does not exist')
  end subroutine error_message_dir_not_exist

  subroutine error_message_file_not_exist (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2003) text
    stop

    2003 format ('Error: File "', a, '" does not exist')
  end subroutine error_message_file_not_exist

  subroutine error_message_var_not_numeric (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2004) text
    stop

    2004 format ('Error: Variable "', a, '" is not a number')
  end subroutine error_message_var_not_numeric

  subroutine error_message_domain_number_exceed (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2005) text
    stop

    2005 format ('Error: Output domain number (', a, ') exceeds 99')
  end subroutine error_message_domain_number_exceed

  subroutine error_message_debug_option (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, 2000) 'Error: debug should be set to "T" or "F"'
    stop

    2000 format (a)
  end subroutine error_message_debug_option

end module module_messages
