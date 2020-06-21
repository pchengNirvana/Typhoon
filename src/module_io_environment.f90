module module_io_environment
  ! Written by P. Cheng
  implicit none

contains

  subroutine read_env_settings
    use module_define_variables
    use module_debug, only : error_message_env_unknown, &
      error_message_env_not_set, error_message_env_not_number
    use module_procedures, only : is_numeric
    implicit none

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
      call error_message_env_unknown(trim(env_name), trim(adjustl(env)))
    end if

    ! read and check if we want to create and use netcdf
    env_name = 'use_netcdf'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') then
      use_netcdf = .false.
    else if (trim(adjustl(env)) == 'T' .or. &
             trim(adjustl(env)) == '1' .or. &
             trim(adjustl(env)) == 'TRUE' .or. &
             trim(adjustl(env)) == 'True' .or. &
             trim(adjustl(env)) == 'true') then
      use_netcdf = .true.
    else if (trim(adjustl(env)) == 'F' .or. &
             trim(adjustl(env)) == '0' .or. &
             trim(adjustl(env)) == 'FALSE' .or. &
             trim(adjustl(env)) == 'False' .or. &
             trim(adjustl(env)) == 'false') then
      use_netcdf = .false.
    else
      call error_message_env_unknown(trim(env_name), trim(adjustl(env)))
    end if

    ! read .dat directory and filename
    env_name = 'dat_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    dat_dir = trim(adjustl(env))
    env_name = 'dat_file'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    dat_file = trim(adjustl(env))

    ! read .ctl directory and filename
    env_name = 'ctl_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    ctl_dir = trim(adjustl(env))
    env_name = 'ctl_file'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    ctl_file = trim(adjustl(env))

    ! if use_netcdf == .true., read .nc directory and filename
    if (use_netcdf) then
      env_name = 'nc_dir'
      call get_environment_variable(trim(env_name), env)
      if (trim(adjustl(env)) == '') &
        call error_message_env_not_set(trim(env_name))
      nc_dir = trim(adjustl(env))
      env_name = 'nc_file'
      call get_environment_variable(trim(env_name), env)
      if (trim(adjustl(env)) == '') &
        call error_message_env_not_set(trim(env_name))
      nc_file = trim(adjustl(env))
    end if

    ! read and check output directory
    env_name = 'out_dir'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    out_dir = trim(adjustl(env))
    env_name = 'out_file'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    out_file = trim(adjustl(env))

    ! read and check nx value
    env_name = 'nx'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) nx
    end if

    ! read and check ny value
    env_name = 'ny'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) ny
    end if

    ! read and check nz/zmax value
    env_name = 'nz'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) nz
    end if
    env_name = 'zmax'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) zmax
    end if

    ! read and check nt/tmax value
    env_name = 'nt'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) nt
    end if
    env_name = 'tmax'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) tmax
    end if

    ! read and check nr value
    env_name = 'nr'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) nr
    end if

    ! read and check nan value
    env_name = 'nan_val'
    call get_environment_variable(trim(env_name), env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set(trim(env_name))
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_env_not_number(trim(env_name), trim(adjustl(env)))
    else
      read(env, *) nan_val
    end if
  end subroutine read_env_settings

end module module_io_environment
