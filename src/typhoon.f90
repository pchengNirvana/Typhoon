program typhoon
  use module_define_constants
  use module_define_variables
  use module_io_environment, only : read_env_settings
  use module_io_main, only : write_r17
  use module_procedures, only : coordinate, symmetric, calculate_r17
  use module_debug, only : debug_in_file, debug_out_file, &
    check_in_file_exists
  implicit none

  ! read environment settings
  call read_env_settings

  ! check and open input file
  dat_dir = trim(dat_dir) !!! dat_dir
  dat_file = trim(typhoon_id)//'_d'//domain//trim(in_file_suffix)
  call check_in_file_exists(trim(dat_dir), trim(dat_file))
  if (debug) call debug_in_file(trim(dat_dir), trim(dat_file))
  open(lun, file = trim(dat_dir)//'/'//trim(dat_file), form = "unformatted", &
    access = 'direct', status = 'old', recl = (nx*ny)*file_recl)

  !!!!!!!!!!! allocate main variable arrays !!!!!!!!!!!
  allocate(u(nx, ny, nz, nt), &
           v(nx, ny, nz, nt), &
           p(nx, ny, nz, nt), &
           w(nx, ny, nz, nt), &
           h(nx, ny, nz, nt), &
           u10(nx, ny, nz, nt), &
           v10(nx, ny, nz, nt), &
           slp(nx, ny, nz, nt), &
           tcx(nz, nt), &
           tcy(nz, nt), &
           smn(nz, nt), &
           ur(nx, ny, nz, nt), &
           vt(nx, ny, nz, nt), &
           vtb(nr, nz, nt), &
           urb(nr, nz, nt), &
           wb(nr, nz, nt), &
           hb(nr, nz, nt))

  !!!!!!!!!!! read data from input file !!!!!!!!!!!
  
  uvrec = 0 ! record location in .dat file

  ! read var #1 - slp
  loop_read_t_1: do t = 1, nt
    loop_read_z_1: do k = 1, nz
      uvrec = uvrec + 1
      read(lun, rec = uvrec) ((slp(i, j, k, t), i = 1, nx), j = 1, ny)
    end do loop_read_z_1
  end do loop_read_t_1

  ! read var #2 - u10
  loop_read_t_2: do t = 1, nt
    loop_read_z_2: do k = 1, nz
      uvrec = uvrec + 1
      read(lun, rec = uvrec) ((u10(i, j, k, t), i = 1, nx), j = 1, ny)
    end do loop_read_z_2
  end do loop_read_t_2

  ! read var #3 - v10
  loop_read_t_3: do t = 1, nt
    loop_read_z_3: do k = 1, nz
      uvrec = uvrec + 1
      read(lun, rec = uvrec) ((v10(i, j, k, t), i = 1, nx), j = 1, ny)
    end do loop_read_z_3
  end do loop_read_t_3

  ! loop over each dimension to find smn, tcx, tcy
  loop_t_1: do t = 1, nt
    loop_z_1: do k = 1, nz
      smn(k, t) = slp(120, 120, k, t) ! *** what is this 120 *** !
      loop_y_1: do j = 1, ny
        loop_x_1: do i = 1, nx
          if (slp(i, j, k, t) .le. smn(k, t)) then
            smn(k, t) = slp(i, j, k, t)
            tcy(k, t) = j
            tcx(k, t) = i
          end if
        end do loop_x_1
      end do loop_y_1
    end do loop_z_1
  end do loop_t_1

  ! debug setting
!  if (debug) then
!    print*, size(tcx)
!    print*, size(tcy)
!    print*, sum(u10), sum(v10), sum(slp)
!  end if

  ! comment place holder
  call coordinate(nx, ny, nz, nt, u10, v10, tcx, tcy, ur, vt)
  call symmetric(nx, ny, nz, nt, nr, tcx, tcy, vt, vtb)

  !!!!!!!!!!! typhoon size (?) r17 !!!!!!!!!!
  allocate(r17(nt))
  call calculate_r17(nz, nt, nr, vtb, r17) ! calculate r17
  out_dir = trim(out_dir) !!! out_dir
  out_file = trim(typhoon_id)//'_'//domain//trim(out_file_suffix) !!! out_file
  if (debug) call debug_out_file(trim(out_dir), trim(out_file))
  call write_r17(trim(out_dir), trim(out_file), nt, r17)


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
