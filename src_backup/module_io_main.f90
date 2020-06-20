module module_io_main
  implicit none

contains

  subroutine read_env_settings
    use module_def_vars
    use module_messages, only : message_text, error_message_env_not_set, &
      error_message_dir_not_exist, error_message_var_not_numeric
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

    ! read and check tdef value
    call get_environment_variable('tdef', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('tdef')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('tdef')
    else
      read(env, '(i3.3)') nt
    end if

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
      call error_message_domain_number_exceed(len(trim(adjustl(env))))
    end if

    ! read wrfout file interval (in hours)
    call get_environment_variable('interval_h', env)
    if (trim(adjustl(env)) == '') call error_message_env_not_set('interval_h')
    if (.not. is_numeric(trim(adjustl(env)))) then
      call error_message_var_not_numeric('interval_h')
    else
      read(env, *) interval_h
    end if
  end subroutine read_env_settings

  subroutine read_wrfout_dimensions &
    (directory, filename, nx, ny, nz, nt, nr)
    use netcdf
    use module_procedures, only: check
    implicit none

    character(len = *), intent (in) :: directory, filename
    integer, intent (out) :: nx, ny, nz, nt, nr

    integer :: ncid
    integer, allocatable :: dimid(:)

    ! allocate arrays
    allocate(dimid(5))

    ! open wrfout file
    call check(nf90_open(directory//'/'//filename, nf90_nowrite, ncid))

    ! read dimensions
    call check(nf90_inq_dimid(ncid, 'Time', dimid(1)))
    call check(nf90_inquire_dimension(ncid, dimid(1), len = nt))
    call check(nf90_inq_dimid(ncid, 'west_east', dimid(2)))
    call check(nf90_inquire_dimension(ncid, dimid(2), len = nx))
    call check(nf90_inq_dimid(ncid, 'south_north', dimid(3)))
    call check(nf90_inquire_dimension(ncid, dimid(3), len = ny))
    call check(nf90_inq_dimid(ncid, 'bottom_top', dimid(4)))
    call check(nf90_inquire_dimension(ncid, dimid(4), len = nz))
    call check(nf90_inq_dimid(ncid, '?????', dimid(5))) ! *** mark this field *** !
    call check(nf90_inquire_dimension(ncid, dimid(5), len = nr))

    ! close wrfout file
    call check(nf90_close(ncid))

    ! deallocate arrays
    if (allocated(dimid)) deallocate(dimid)

  end subroutine read_wrfout_dimensions

  subroutine read_wrfout_variables &
    (directory, filename, nx, ny, nz, nt, nr, u, v, p, w, h, u10, v10, slp)
    use netcdf
    use module_procedures, only: check
    implicit none

    character(len = *), intent (in) :: directory, filename
    integer, intent (in) :: nx, ny, nz, nt, nr
    real(4), intent (out), optional :: u(nx, ny, nz, nt)
    real(4), intent (out), optional :: v(nx, ny, nz, nt)
    real(4), intent (out), optional :: p(nx, ny, nz, nt)
    real(4), intent (out), optional :: w(nx, ny, nz, nt)
    real(4), intent (out), optional :: h(nx, ny, nz, nt)
    real(4), intent (out), optional :: u10(nx, ny, nt)
    real(4), intent (out), optional :: v10(nx, ny, nt)
    real(4), intent (out), optional :: slp(nx, ny, nt)
    
    integer :: ncid
    integer, allocatable :: varid(:)
    real(4) :: u10_orig(nx, ny, nt), v10_orig(nx, ny, nt)
    real(4) :: u_orig(nx+1, ny, nz, nt), v_orig(nx, ny+1, nz, nt)
    real(4) :: w_orig(nx, ny, nz+1, nt)
    real(4) :: u_temp(nx+1, ny, nz, nt), v_temp(nx, ny+1, nz, nt)
    real(4) :: sinalpha(nx, ny, nt), cosalpha(nx, ny, nt)

    ! allocate arrays
    allocate(varid(8))

    ! open wrfout file
    call check(nf90_open(directory//'/'//filename, nf90_nowrite, ncid))

    ! read variables
    call check(nf90_inq_varid(ncid, 'U', varid(1)))
    call check(nf90_get_var(ncid, varid(1), u_orig))
    call check(nf90_inq_varid(ncid, 'V', varid(2)))
    call check(nf90_get_var(ncid, varid(2), v_orig))
    call check(nf90_inq_varid(ncid, '?????', varid(3)))
    call check(nf90_get_var(ncid, varid(3), p))
    call check(nf90_inq_varid(ncid, 'W', varid(4)))
    call check(nf90_get_var(ncid, varid(4), w_orig))
    call check(nf90_inq_varid(ncid, '?????', varid(5)))
    call check(nf90_get_var(ncid, varid(5), h))
    call check(nf90_inq_varid(ncid, 'U10', varid(6)))
    call check(nf90_get_var(ncid, varid(6), u10_orig))
    call check(nf90_inq_varid(ncid, 'V10', varid(7)))
    call check(nf90_get_var(ncid, varid(7), v10_orig))
    call check(nf90_inq_varid(ncid, 'PSFC', varid(8)))
    call check(nf90_get_var(ncid, varid(8), slp))
    call check(nf90_inq_varid(ncid, 'SINALPHA', varid(9)))
    call check(nf90_get_var(ncid, varid(9), sinalpha))
    call check(nf90_inq_varid(ncid, 'COSALPHA', varid(10)))
    call check(nf90_get_var(ncid, varid(10), cosalpha))

    ! interpolate wind
    u_temp(:, :, :, :) = .5 * (u_orig(1:nx, :, :, :) + u_orig(2:nx+1, :, :, :))
    v_temp(:, :, :, :) = .5 * (v_orig(:, 1:ny, :, :) + v_orig(:, 2:ny+1, :, :))
    w(:, :, :, :) = .5 * (w_orig(:, :, 1:nz, :) + w_orig(:, :, 2:nz+1, :))

    ! rotate winds
    u10 = u10_orig * cosalpha - v10_orig * sinalpha
    v10 = v10_orig * cosalpha + u10_orig * sinalpha
    u = u_temp * cosalpha - v_temp * sinalpha
    v = v_temp * cosalpha + u_temp * sinalpha

    ! close wrfout file
    call check(nf90_close(ncid))

    ! deallocate arrays
    if (allocated(varid)) deallocate(varid)

  end subroutine read_wrfout_variables

end module module_io_main
