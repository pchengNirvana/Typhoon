module module_io_main
  implicit none

contains

  subroutine read_env_settings
    use module_def_vars
    use module_messages, only : message_text, error_message_env_not_set, &
      error_message_dir_not_exist, error_message_var_not_numeric, &
      error_message_domain_number_exceed, error_message_text
    use module_procedures, only : is_numeric
    implicit none

    ! read and check data directory
    call get_environment_variable('data_dir', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('data_dir')
    data_dir = trim(adjustl(env))
    inquire(file = trim(data_dir), exist = exists)
    if (.not. exists) call error_message_dir_not_exist(trim(data_dir))

    ! read and check output directory
    call get_environment_variable('out_dir', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('out_dir')
    out_dir = trim(adjustl(env))
    inquire(file = trim(out_dir), exist = exists)
    if (.not. exists) then
      call system('mkdir -p '//trim(out_dir))
      call message_text('Creating output directory: '//trim(out_dir))
    end if

    ! read and check typhoon_id
    call get_environment_variable('typhoon_id', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('typhoon_id')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('typhoon_id')
    else
      typhoon_id = trim(env)
    end if

    ! read and check nt value
    call get_environment_variable('nt', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('nt')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('nt')
    else
      read(env, *) nt
    end if

    ! read and check nx value
    call get_environment_variable('nx', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('nx')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('nx')
    else
      read(env, *) nx
    end if

    ! read and check ny value
    call get_environment_variable('ny', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('ny')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('ny')
    else
      read(env, *) ny
    end if

    ! read and check nz value
    call get_environment_variable('nz', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('nz')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('nz')
    else
      read(env, *) nz
    end if

    ! read and check nr value
    call get_environment_variable('nr', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('nr')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('nr')
    else
      read(env, *) nr
    end if

    ! read and check nz value
    call get_environment_variable('file_recl', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('file_recl')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('file_recl')
    else
      read(env, *) file_recl
    end if

    ! read and check input filename suffix
    call get_environment_variable('in_file_suffix', env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set('in_file_suffix')
    in_file_suffix = trim(adjustl(env))

    ! read and check output filename suffix
    call get_environment_variable('out_file_suffix', env)
    if (trim(adjustl(env)) == '') &
      call error_message_env_not_set('out_file_suffix')
    out_file_suffix = trim(adjustl(env))

    ! read domain number to be procesed
    call get_environment_variable('domain', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('domain')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('domain')
    end if
    if (len(trim(adjustl(env))) .eq. 1) then
      domain = '0'//trim(adjustl(env))
    else if (len(trim(adjustl(env))) .eq. 2) then
      domain = trim(adjustl(env))
    else
      call error_message_domain_number_exceed(trim(adjustl(env)))
    end if

    ! read and check debug option
    call get_environment_variable('debug', env)
    if (trim(adjustl(env)) == '') then
      debug = .false.
    else if (trim(adjustl(env)) == 'T') then
      debug = .true.
    else if (trim(adjustl(env)) == 'F') then
      debug = .false.
    else
      call error_message_text('Error: debug should be set to "T" or "F"')
    end if

  end subroutine read_env_settings

end module module_io_main
