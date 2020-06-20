program typhoon
  use module_def_vars
  use module_io_main, only : read_env_settings, read_wrfout_dimensions, &
    read_wrfout_variables
  implicit none

  ! read environment settings
  call read_env_settings

  ! read wrfout files
  loop_over_wrfout_files: do it = 1, ceiling(1.*nt/interval_h) ! *** check time step ***
    ! set wrfout directory and wrfout filename
    ! wrfout_dir = 
    call julian_date(year, month, day, jcdate)
    write(syear,  '(i4.4)') year
    write(smonth, '(i2.2)') month
    write(sday,   '(i2.2)') day
    write(shour,  '(i2.2)') hour
    wrf_timestring = syear//'_'//smonth//'_'//sday
    wrfout_file = 'wrfout_d'//domain//'_'//wrf_timestring//'_'//shour//':00:00'

    if (it .eq. 1) then
      ! read wrfout dimensions if at the beginning
      call read_wrfout_dimensions(trim(wrfout_dir), trim(wrfout_file), &
        nx, ny, nz, nt_temp, nr) ! *** what is nr ***

      ! allocate main variable arrays to store wrfout variables
      allocate(u(nx, ny, nz, nt), & ! U - west_east interp + rotate
               v(nx, ny, nz, nt), & ! V - south_north interp + rotate
               p(nx, ny, nz, nt), & ! P - unstaggered ! *** what is this *** !
               w(nx, ny, nz, nt), & ! W - bottom_top interp
               h(nx, ny, nz, nt), & ! *** what is this *** !
               u10(nx, ny, nt), &   ! U10 - one vertical layer
               v10(nx, ny, nt), &   ! V10 - one vertical layer
               slp(nx, ny, nt))     ! PSFC - one vertical layer
    end if

    ! allocate temporary variable arrays
    allocate(u_temp(nx, ny, nz, nt_temp), & ! *** unused array *** !
             v_temp(nx, ny, nz, nt_temp), & ! *** unused array *** !
             p_temp(nx, ny, nz, nt_temp), & ! *** unused array *** !
             w_temp(nx, ny, nz, nt_temp), & ! *** unused array *** !
             h_temp(nx, ny, nz, nt_temp), & ! *** unused array *** !
             u10_temp(nx, ny, nt_temp), &
             v10_temp(nx, ny, nt_temp), &
             slp_temp(nx, ny, nt_temp))

    ! read each wrfout file
    call read_wrfout_variables(trim(wrfout_dir), trim(wrfout_file), &
      nx, ny, nz, nt_temp, nr, u = u_temp, v = v_temp, p = p_temp, &
      w = w_temp, h = h_temp, u10 = u10_temp, v10 = v10_temp, slp = slp_temp)

    ! write temporary variable arrays to main variable arrays
    u(:, :, :, it:it+nt_temp-1) = u_temp(:, :, :, 1:nt_temp)
    v(:, :, :, it:it+nt_temp-1) = v_temp(:, :, :, 1:nt_temp)
    p(:, :, :, it:it+nt_temp-1) = p_temp(:, :, :, 1:nt_temp)
    w(:, :, :, it:it+nt_temp-1) = w_temp(:, :, :, 1:nt_temp)
    h(:, :, :, it:it+nt_temp-1) = h_temp(:, :, :, 1:nt_temp)
    u10(:, :, it:it+nt_temp-1) = u10_temp(:, :, 1:nt_temp)
    v10(:, :, it:it+nt_temp-1) = v10_temp(:, :, 1:nt_temp)
    slp(:, :, it:it+nt_temp-1) = slp_temp(:, :, 1:nt_temp)

    if (allocated(u_temp)) deallocate(u_temp)
    if (allocated(v_temp)) deallocate(v_temp)
    if (allocated(p_temp)) deallocate(p_temp)
    if (allocated(w_temp)) deallocate(w_temp)
    if (allocated(h_temp)) deallocate(h_temp)
    if (allocated(u10_temp)) deallocate(u10_temp)
    if (allocated(v10_temp)) deallocate(v10_temp)
    if (allocated(slp_temp)) deallocate(slp_temp)
  end do loop_over_wrfout_files

  ! allocate arrays
  allocate(smn(nt), &
           r17(nt), &
           tcx(nt), &
           tcy(nt))

  ! loop over each dimension
  loop_t_1: do l = 1, nt
    loop_z_1: do k = 1, 1 ! nz
      smn(l) = slp(120, 120, l) ! *** what is this 120 *** !
      loop_y_1: do j = 1, ny
        loop_x_1: do i = 1, nx
          if (slp(i, j, l) .le. smn(l)) then
            smn(l) = slp(i, j, l)
            tcy(l) = j
            tcx(l) = i
          end if
        end do loop_x_1
      end do loop_y_1
    end do loop_z_1
  end do loop_t_1
  write(*, *) tcx
  write(*, *) tcy

  ! allocate arrays
  allocate(ur(nx, ny, nz, nt), &
           vt(nx, ny, nz, nt), &
           vtb(nr, nz, nt), &
           urb(nr, nz, nt), & ! *** unused array *** !
           wb(nr, nz, nt), & ! *** unused array *** !
           hb(nr, nz, nt)) ! *** unused array *** !

  ! comment place holder
  ! call coordinate(u10, v10, ur, vt, nx, ny, nz, nt, tcx, tcy)
  ! call symmetric(vt, vtb, nx, ny, nz, nt, tcx, tcy, nr)

  ! calculate r17, loop over nt, nz, nr
  r17(:) = 0.
  loop_t_2: do l = 1, nt
    loop_z_2: do k = 1, 1 ! nz
      loop_r_2: do r = 1, nr-1
        if ((vtb(r, k, l) .gt. 17) .and. (vtb(r+1, k, l) .lt. 17)) then
          r17(l) = r * 2.
        end if
      end do loop_r_2
    end do loop_z_2
  end do loop_t_2

  ! open output file
  out_file = trim(typhoon_id)//'_'//domain//trim(out_file_suffix)
  open(lun, file = trim(out_dir)//'/'//trim(out_file), status = 'replace')

  ! write r17 time series to file
  do l = 1, nt
    write(lun, *) r17(l)
  end do

  ! close output file
  close(lun)

  ! deallocate arrays
  if (allocated(u)) deallocate(u)
  if (allocated(v)) deallocate(v)
  if (allocated(p)) deallocate(p)
  if (allocated(w)) deallocate(w)
  if (allocated(h)) deallocate(h)
  if (allocated(u10)) deallocate(u10)
  if (allocated(v10)) deallocate(v10)
  if (allocated(slp)) deallocate(slp)
  if (allocated(tcx)) deallocate(tcx)
  if (allocated(tcy)) deallocate(tcy)
  if (allocated(smn)) deallocate(smn)
  if (allocated(r17)) deallocate(r17)
  if (allocated(ur)) deallocate(ur)
  if (allocated(vt)) deallocate(vt)
  if (allocated(vtb)) deallocate(vtb)
  if (allocated(urb)) deallocate(urb)
  if (allocated(wb)) deallocate(wb)
  if (allocated(hb)) deallocate(hb)

end program typhoon
