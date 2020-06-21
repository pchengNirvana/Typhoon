module module_io_environment
  implicit none

contains

  subroutine read_env_settings
    use module_define_variables
    use module_debug, only : message_text, check_in_directory_exists, &
      check_out_directory_exists, &
      error_message_env_not_set, &
      error_message_var_not_numeric, &
      error_message_domain_number_exceed, error_message_unknown
    use module_procedures, only : is_numeric
    implicit none

    ! read and check .dat directory
    env_name = 'dat_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    dat_dir = trim(adjustl(env))
    call check_in_directory_exists(trim(dat_dir))

    ! read and check .ctl directory
    env_name = 'ctl_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    dat_dir = trim(adjustl(env))
    call check_in_directory_exists(trim(dat_dir))

    ! read and check output directory
    env_name = 'out_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    out_dir = trim(adjustl(env))
    call check_out_directory_exists(trim(out_dir))

    ! read and check typhoon_id
    env_name = 'typhoon_id'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      typhoon_id = trim(env)
    end if

    ! read and check nt value
    env_name = 'nt'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) nt
    end if

    ! read and check nx value
    env_name = 'nx'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) nx
    end if

    ! read and check ny value
    env_name = 'ny'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) ny
    end if

    ! read and check nz value
    env_name = 'nz'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) nz
    end if

    ! read and check nr value
    env_name = 'nr'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) nr
    end if

    ! read and check nz value
    env_name = 'file_recl'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric(trim(env_name))
    else
      read(env, *) file_recl
    end if

    ! read and check input filename suffix
    env_name = 'in_file_suffix'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    in_file_suffix = trim(adjustl(env))

    ! read and check output filename suffix
    env_name = 'out_file_suffix'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    out_file_suffix = trim(adjustl(env))

    ! read domain number to be procesed
    env_name = 'domain'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) &
      call error_message_var_not_numeric(trim(env_name))
    if (len(trim(adjustl(env))) .eq. 1) then
      domain = '0'//trim(adjustl(env))
    else if (len(trim(adjustl(env))) .eq. 2) then
      domain = trim(adjustl(env))
    else
      call error_message_domain_number_exceed(trim(adjustl(env)))
    end if

    ! read and check debug option
    env_name = 'debug'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') then
      debug = .false.
    else if (trim(adjustl(env)) == 'T' .or. &
             trim(adjustl(env)) == '1' .or. &
             trim(adjustl(env)) == 'TRUE' .or. &
             trim(adjustl(env)) == 'True' .or. &
             trim(adjustl(env)) == 'true') then
      debug = .true.
    else if (trim(adjustl(env)) == 'F' .or. &
             trim(adjustl(env)) == '0' .or. &
             trim(adjustl(env)) == 'FALSE' .or. &
             trim(adjustl(env)) == 'False' .or. &
             trim(adjustl(env)) == 'false') then
      debug = .false.
    else
      call error_message_unknown(trim(env_name), trim(adjustl(env)))
    end if
  end subroutine read_env_settings

end module module_io_environment
