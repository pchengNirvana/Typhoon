program typhoon
  ! Written by P. Cheng
  use module_define_constants
  use module_define_variables
  use module_io_environment, only : read_env_settings
  use module_io_main, only : read_dat_file, write_r17
  use module_procedures, only : coordinate, symmetric, calculate_r17, &
    find_tropical_cyclone_center
  use module_debug, only : debug_in_file, check_in_file_exists, &
    debug_out_file, check_out_directory_exists, debug_dimension_settings, &
    debug_check_read_variables, debug_tropical_cyclone_center
  implicit none

  ! read environment settings
  call read_env_settings
  if (debug) call debug_dimension_settings(nx, ny, nz, nt, nr)

  !!!!!!!!!!!!!!!!!!! allocate main variable arrays !!!!!!!!!!!!!!!!!!!
  !!!!!!!! can be savely uncomment to save memory if not needed !!!!!!!
  !!!!! allocate 2D arrays !!!!!
  !allocate(xlat(nx, ny, nt))
  !allocate(xlong(nx, ny, nt))
  !allocate(q2(nx, ny, nt))
  !allocate(t2(nx, ny, nt))
  !allocate(psfc(nx, ny, nt))
  allocate(u10(nx, ny, nt))
  allocate(v10(nx, ny, nt))
  !allocate(hgt(nx, ny, nt))
  !allocate(tsk(nx, ny, nt))
  !allocate(rainc(nx, ny, nt))
  !allocate(rainnc(nx, ny, nt))
  !allocate(hfx(nx, ny, nt))
  !allocate(qfx(nx, ny, nt))
  !allocate(lh(nx, ny, nt))
  !allocate(sst(nx, ny, nt))
  allocate(slp(nx, ny, nt))
  !allocate(maxdbz(nx, ny, nt))
  !!!!! allocate 3D arrays !!!!!
  !allocate(u(nx, ny, nz, nt))
  !allocate(v(nx, ny, nz, nt))
  !allocate(w(nx, ny, nz, nt))
  !allocate(qv(nx, ny, nz, nt))
  !allocate(qc(nx, ny, nz, nt))
  !allocate(qr(nx, ny, nz, nt))
  !allocate(qi(nx, ny, nz, nt))
  !allocate(qs(nx, ny, nz, nt))
  !allocate(qg(nx, ny, nz, nt))
  !allocate(hdiabatic(nx, ny, nz, nt))
  !allocate(height(nx, ny, nz, nt))
  !allocate(tk(nx, ny, nz, nt))
  !allocate(theta(nx, ny, nz, nt))
  !allocate(rh(nx, ny, nz, nt))
  !allocate(dbz(nx, ny, nz, nt))

  !!!!!!!!!!!!!!!!!! check and read from input file !!!!!!!!!!!!!!!!!!
  if (.not. use_netcdf) then
    call check_in_file_exists(trim(dat_dir), trim(dat_file))
    if (debug) call debug_in_file(trim(dat_dir), trim(dat_file))
!    call read_dat_file(trim(dat_dir), trim(dat_file), nx, ny, nz, zmax, nt, &
!      file_recl, u10 = u10, v10 = v10, slp = slp)
   if (debug) then
      call debug_check_read_variables(nx, ny, nz, nt, nan_val, &
        u10 = u10, v10 = v10, slp = slp)
    end if
  else
    call check_in_file_exists(trim(nc_dir), trim(nc_file))
    if (debug) call debug_in_file(trim(nc_dir), trim(nc_file))
  end if

  !!!!!!!!!!!!!!!! now start calculating derived data !!!!!!!!!!!!!!!!!
  ! allocate derived array
  allocate(tcx(nz, nt))
  allocate(tcy(nz, nt))
  allocate(smn(nz, nt))
  allocate(ur(nx, ny, nz, nt))
  allocate(vt(nx, ny, nz, nt))
  allocate(vtb(nr, nz, nt))
  !allocate(urb(nr, nz, nt))
  allocate(r17(nt))

  ! assign fake arrays 3D => 4D
  slp_fake(1:nx, 1:ny, 1:1, 1:nt) => slp(:, :, :)
  u10_fake(1:nx, 1:ny, 1:1, 1:nt) => u10(:, :, :)
  v10_fake(1:nx, 1:ny, 1:1, 1:nt) => v10(:, :, :)

  ! fine indices and pressure of tropical cyclone center
  call find_tropical_cyclone_center(nx, ny, nz, nt, slp_fake, tcx, tcy, smn)
  if (debug) call debug_tropical_cyclone_center(nz, nt, tcx, tcy, smn)

  ! convert u10/v10 to polar coordinate
  call coordinate(nx, ny, nz, nt, u10_fake, v10_fake, tcx, tcy, ur, vt)
  call symmetric(nx, ny, nz, nt, nr, tcx, tcy, vt, vtb)

  ! call subroutine to calculate r17
  call calculate_r17(nz, nt, nr, vtb, r17)

  !!!!!!!!!!!!!!! now write final results to out_file !!!!!!!!!!!!!!!!
  if (debug) call debug_out_file(trim(out_dir), trim(out_file))
  call write_r17(trim(out_dir), trim(out_file), nt, r17)


  ! deallocate arrays, use if statement to make sure it won't attempt to 
  ! deallocate arrays when not being allocated
  ! derived variables
  if (allocated(tcx)) deallocate(tcx)
  if (allocated(tcy)) deallocate(tcy)
  if (allocated(smn)) deallocate(smn)
  if (allocated(r17)) deallocate(r17)
  if (allocated(ur)) deallocate(ur)
  if (allocated(vt)) deallocate(vt)
  if (allocated(urb)) deallocate(urb)
  if (allocated(vtb)) deallocate(vtb)
  ! 2D .dat variables
  if (allocated(xlat)) deallocate(xlat)
  if (allocated(xlong)) deallocate(xlong)
  if (allocated(q2)) deallocate(q2)
  if (allocated(t2)) deallocate(t2)
  if (allocated(psfc)) deallocate(psfc)
  if (allocated(u10)) deallocate(u10)
  if (allocated(v10)) deallocate(v10)
  if (allocated(hgt)) deallocate(hgt)
  if (allocated(tsk)) deallocate(tsk)
  if (allocated(rainc)) deallocate(rainc)
  if (allocated(rainnc)) deallocate(rainnc)
  if (allocated(hfx)) deallocate(hfx)
  if (allocated(qfx)) deallocate(qfx)
  if (allocated(lh)) deallocate(lh)
  if (allocated(sst)) deallocate(sst)
  if (allocated(slp)) deallocate(slp)
  if (allocated(maxdbz)) deallocate(maxdbz)
  ! 3D .dat variables
  if (allocated(u)) deallocate(u)
  if (allocated(v)) deallocate(v)
  if (allocated(w)) deallocate(w)
  if (allocated(qv)) deallocate(qv)
  if (allocated(qc)) deallocate(qc)
  if (allocated(qr)) deallocate(qr)
  if (allocated(qi)) deallocate(qi)
  if (allocated(qs)) deallocate(qs)
  if (allocated(qg)) deallocate(qg)
  if (allocated(hdiabatic)) deallocate(hdiabatic)
  if (allocated(height)) deallocate(height)
  if (allocated(tk)) deallocate(tk)
  if (allocated(theta)) deallocate(theta)
  if (allocated(rh)) deallocate(rh)
  if (allocated(dbz)) deallocate(dbz)

end program typhoon
