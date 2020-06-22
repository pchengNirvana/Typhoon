module module_io_main
  ! Written by P. Cheng
  implicit none

contains

  subroutine read_dat_file &
    (directory, filename, nx, ny, nz, zmax, nt, file_recl, &
     xlat, xlong, q2, t2, psfc, u10, v10, hgt, tsk, rainc, rainnc, &
     hfx, qfx, lh, sst, slp, maxdbz, u, v, w, qv, qc, qr, qi, qs, qg, &
     hdiabatic, height, tk, theta, rh, dbz)
    ! Written by P. Cheng based on bianliang_GR_try.f90
    implicit none

    character(len = *), intent (in) :: directory, filename
    integer, intent (in) :: nx, ny, nz, zmax, nt, file_recl
    ! 2D variables
    real(4), intent (out), optional :: xlat(nx, ny, nt)
    real(4), intent (out), optional :: xlong(nx, ny, nt)
    real(4), intent (out), optional :: q2(nx, ny, nt)
    real(4), intent (out), optional :: t2(nx, ny, nt)
    real(4), intent (out), optional :: psfc(nx, ny, nt)
    real(4), intent (out), optional :: u10(nx, ny, nt)
    real(4), intent (out), optional :: v10(nx, ny, nt)
    real(4), intent (out), optional :: hgt(nx, ny, nt)
    real(4), intent (out), optional :: tsk(nx, ny, nt)
    real(4), intent (out), optional :: rainc(nx, ny, nt)
    real(4), intent (out), optional :: rainnc(nx, ny, nt)
    real(4), intent (out), optional :: hfx(nx, ny, nt)
    real(4), intent (out), optional :: qfx(nx, ny, nt)
    real(4), intent (out), optional :: lh(nx, ny, nt)
    real(4), intent (out), optional :: sst(nx, ny, nt)
    real(4), intent (out), optional :: slp(nx, ny, nt)
    real(4), intent (out), optional :: maxdbz(nx, ny, nt)
    ! 3D variables
    real(4), intent (out), optional :: u(nx, ny, nz, nt)
    real(4), intent (out), optional :: v(nx, ny, nz, nt)
    real(4), intent (out), optional :: w(nx, ny, nz, nt)
    real(4), intent (out), optional :: qv(nx, ny, nz, nt)
    real(4), intent (out), optional :: qc(nx, ny, nz, nt)
    real(4), intent (out), optional :: qr(nx, ny, nz, nt)
    real(4), intent (out), optional :: qi(nx, ny, nz, nt)
    real(4), intent (out), optional :: qs(nx, ny, nz, nt)
    real(4), intent (out), optional :: qg(nx, ny, nz, nt)
    real(4), intent (out), optional :: hdiabatic(nx, ny, nz, nt)
    real(4), intent (out), optional :: height(nx, ny, nz, nt)
    real(4), intent (out), optional :: tk(nx, ny, nz, nt)
    real(4), intent (out), optional :: theta(nx, ny, nz, nt)
    real(4), intent (out), optional :: rh(nx, ny, nz, nt)
    real(4), intent (out), optional :: dbz(nx, ny, nz, nt)

    integer :: lun = 100
    integer :: i, j, k, l, tb

    tb = 17 + 15*zmax

    ! open input file
    open(lun, file = directory//'/'//filename, form = 'unformatted', &
      access = 'direct', recl = nx * ny * file_recl, convert = 'big_endian')

    ! loop over time dimension
    loop_t: do l = 1, nt 
      ! read 2D variables
      if (present(xlat)) then
        read(lun, rec = 0*zmax + 0 + 1 + (l-1)*tb) &
          ((xlat(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(xlong)) then
        read(lun, rec = 0*zmax + 1 + 1 + (l-1)*tb) &
          ((xlong(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(q2)) then
        read(lun, rec = 3*zmax + 2 + 1 + (l-1)*tb) &
          ((q2(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(t2)) then
        read(lun, rec = 3*zmax + 3 + 1 + (l-1)*tb) &
          ((t2(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(psfc)) then
        read(lun, rec = 3*zmax + 4 + 1 + (l-1)*tb) &
          ((psfc(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(u10)) then
        read(lun, rec = 3*zmax + 5 + 1 + (l-1)*tb) &
          ((u10(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(v10)) then
        read(lun, rec = 3*zmax + 6 + 1 + (l-1)*tb) &
          ((v10(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(hgt)) then
        read(lun, rec = 10*zmax + 7 + 1 + (l-1)*tb) &
          ((hgt(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(tsk)) then
        read(lun, rec = 10*zmax + 8 + 1 + (l-1)*tb) &
          ((tsk(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(rainc)) then
        read(lun, rec = 10*zmax + 9 + 1 + (l-1)*tb) &
          ((rainc(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(rainnc)) then
        read(lun, rec = 10*zmax + 10 + 1 + (l-1)*tb) &
          ((rainnc(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(hfx)) then
        read(lun, rec = 10*zmax + 11 + 1 + (l-1)*tb) &
          ((hfx(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(qfx)) then
        read(lun, rec = 10*zmax + 12 + 1 + (l-1)*tb) &
          ((qfx(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(lh)) then
        read(lun, rec = 10*zmax + 13 + 1 + (l-1)*tb) &
          ((lh(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(sst)) then
        read(lun, rec = 10*zmax + 14 + 1 + (l-1)*tb) &
          ((sst(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(slp)) then
        read(lun, rec = 14*zmax + 15 + 1 + (l-1)*tb) &
          ((slp(i, j, l), i = 1, nx), j = 1, ny)
      end if
      if (present(maxdbz)) then
        read(lun, rec = 15*zmax + 16 + 1 + (l-1)*tb) &
          ((maxdbz(i, j, l), i = 1, nx), j = 1, ny)
      end if

      ! read 3D variables
      ! loop over vertical dimension
      loop_z: do k = 1, nz
        if (present(u)) then
          read(lun, rec = 0*zmax + 2 + k + (l-1)*tb) &
            ((u(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(v)) then
          read(lun, rec = 1*zmax + 2 + k + (l-1)*tb) &
            ((v(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(w)) then
          read(lun, rec = 2*zmax + 2 + k + (l-1)*tb) &
            ((w(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qv)) then
          read(lun, rec = 3*zmax + 7 + k + (l-1)*tb) &
            ((qv(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qc)) then
          read(lun, rec = 4*zmax + 7 + k + (l-1)*tb) &
            ((qc(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qr)) then
          read(lun, rec = 5*zmax + 7 + k + (l-1)*tb) &
            ((qr(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qi)) then
          read(lun, rec = 6*zmax + 7 + k + (l-1)*tb) &
            ((qi(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qs)) then
          read(lun, rec = 7*zmax + 7 + k + (l-1)*tb) &
            ((qs(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(qg)) then
          read(lun, rec = 8*zmax + 7 + k + (l-1)*tb) &
            ((qg(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(hdiabatic)) then
          read(lun, rec = 9*zmax + 7 + k + (l-1)*tb) &
            ((hdiabatic(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(height)) then
          read(lun, rec = 10*zmax + 15 + k + (l-1)*tb) &
            ((height(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(tk)) then
          read(lun, rec = 11*zmax + 15 + k + (l-1)*tb) &
            ((tk(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(theta)) then
          read(lun, rec = 12*zmax + 15+k + (l-1)*tb) &
            ((theta(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(rh)) then
          read(lun, rec = 13*zmax + 15+k + (l-1)*tb) &
            ((rh(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
        if (present(dbz)) then
          read(lun, rec = 14*zmax + 16+k + (l-1)*tb) &
            ((dbz(i, j, k, l), i = 1, nx), j = 1, ny)
        end if
      end do loop_z
    end do loop_t

    ! close input file
    close(lun)
  end subroutine read_dat_file

  subroutine write_r17 &
    (directory, filename, nt, r17)
    implicit none

    character(len = *), intent (in) :: directory, filename
    integer, intent (in) :: nt
    integer, intent (in) :: r17(nt)

    integer :: lun
    integer :: t

    ! open output file
    open(lun, file = directory//'/'//filename, status = 'replace')

    ! write r17 to output file
    do t = 1, nt
      write(lun, *) r17(t)
    end do

    ! close output file
    close(lun)
  end subroutine write_r17

end module module_io_main
