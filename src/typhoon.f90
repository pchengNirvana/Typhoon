program typhoon
  use module_def_vars
  use module_io_main, only : read_env_settings
  use module_messages, only : error_message_file_not_exist
  implicit none

  ! read environment settings
  call read_env_settings

  ! check and open input file
  in_dir = trim(data_dir)//'/'//trim(typhoon_id)
  in_file = trim(typhoon_id)//'_d'//domain//trim(in_file_suffix)
  inquire(file = trim(in_dir)//'/'//trim(in_file), exist = exists)
  if (.not. exists) call error_message_file_not_exist(trim(in_dir)//'/'// &
    trim(in_file))
  open(lun, file = trim(in_dir)//'/'//trim(in_file), form = "unformatted", &
    access = 'direct', status = 'old', recl = (nx*ny)*file_recl)

  ! allocate main variable arrays to store wrfout variables
  allocate(u(nx, ny, nz, nt), & ! *** unused array *** !
           v(nx, ny, nz, nt), & ! *** unused array *** !
           p(nx, ny, nz, nt), & ! *** unused array *** !
           w(nx, ny, nz, nt), & ! *** unused array *** !
           h(nx, ny, nz, nt), & ! *** unused array *** !
           u10(nx, ny, nz, nt), &
           v10(nx, ny, nz, nt), &
           slp(nx, ny, nz, nt), &
           tcx(nz, nt), &
           tcy(ny, nt), &
           smn(nz, nt), & ! *** can dim 1 be nz or has to be 1 *** !
           r17(nt), &
           ur(nx, ny, nz, nt), &
           vt(nx, ny, nz, nt), &
           vtb(nr, nz, nt), &
           urb(nr, nz, nt), & ! *** unused array *** !
           wb(nr, nz, nt), &  ! *** unused array *** !
           hb(nr, nz, nt))    ! *** unused array *** !

  ! read data from input file
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
  if (debug) then
    write(*, *) tcx
    write(*, *) tcy
  end if

  ! comment place holder
  ! call coordinate(u10, v10, ur, vt, nx, ny, nz, nt, tcx, tcy)
  ! call symmetric(vt, vtb, nx, ny, nz, nt, tcx, tcy, nr)

  ! calculate typhoon size (?) r17
  r17(:) = 0.
  loop_t_2: do t = 1, nt
    loop_z_2: do k = 1, nz
      loop_r_2: do i = 1, nr-1
        if ((vtb(i, k, t) .gt. 17) .and. (vtb(i+1, k, t) .lt. 17)) then
          r17(t) = i * 2.
        end if
      end do loop_r_2
    end do loop_z_2
  end do loop_t_2

  ! open output file
  out_file = trim(typhoon_id)//'_'//domain//trim(out_file_suffix)
  open(lun, file = trim(out_dir)//'/'//trim(out_file), status = 'replace')

  ! write r17 time series to file
  do t = 1, nt
    write(lun, *) r17(t)
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
