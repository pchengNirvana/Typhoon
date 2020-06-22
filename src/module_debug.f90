module module_debug
  ! Written by P. Cheng
  implicit none

contains

  subroutine message_text &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
  end subroutine message_text

  subroutine debug_dimension_settings &
    (nx, ny, nz, nt, nr)
    implicit none

    integer, intent (in) :: nx, ny, nz, nt, nr

    write(*, '(a)') 'DEBUG: Current dimension setting is: '
    write(*, '(a, i4)') '       nx = ', nx
    write(*, '(a, i4)') '       ny = ', ny
    write(*, '(a, i4)') '       nz = ', nz
    write(*, '(a, i4)') '       nt = ', nt
    write(*, '(a, i4)') '       nr = ', nr
  end subroutine debug_dimension_settings

  subroutine check_in_file_exists &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    logical :: exists

    inquire(file = directory//'/'//filename, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'ERROR: Input file does not exist: '//&
        directory//'/'//filename
      stop
    end if
  end subroutine check_in_file_exists

  subroutine check_in_directory_exists &
    (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'ERROR: Input file directory does not exist: '//directory
      stop
    end if
  end subroutine check_in_directory_exists

  subroutine debug_in_file &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Input file: '//directory//'/'//filename
  end subroutine debug_in_file

  subroutine debug_check_2D_variable_values &
    (varname, time, varmax, varmin, varave)
    implicit none

    character(len = *), intent (in) :: varname
    integer, intent (in) :: time
    real(4), intent (in) :: varmax, varmin, varave

    write(*, '(5x, a10, ",", i6, ",", f8.2, ",", f8.2, ",", f8.2)') &
      varname, time, varmax, varmin, varave
  end subroutine debug_check_2D_variable_values

  subroutine debug_check_3D_variable_values &
    (varname, time, height, varmax, varmin, varave)
    implicit none

    character(len = *), intent (in) :: varname
    integer, intent (in) :: time, height
    real(4), intent (in) :: varmax, varmin, varave

    write(*, '(5x, a10, ",", i6, ",  ", i6, ",", f8.2, ",", f8.2, ",", f8.2)') &
      varname, time, height, varmax, varmin, varave
  end subroutine debug_check_3D_variable_values

  subroutine debug_check_read_variables &
    (nx, ny, nz, nt, nan_val, xlat, xlong, q2, t2, psfc, u10, v10, hgt, tsk, &
     rainc, rainnc, hfx, qfx, lh, sst, slp, maxdbz, u, v, w, qv, qc, &
     qr, qi, qs, qg, hdiabatic, height, tk, theta, rh, dbz)
    implicit none

    integer, intent (in) :: nx, ny, nz, nt
    real(4), intent (in) :: nan_val
    ! 2D variables
    real(4), intent (in), optional :: xlat(nx, ny, nt)
    real(4), intent (in), optional :: xlong(nx, ny, nt)
    real(4), intent (in), optional :: q2(nx, ny, nt)
    real(4), intent (in), optional :: t2(nx, ny, nt)
    real(4), intent (in), optional :: psfc(nx, ny, nt)
    real(4), intent (in), optional :: u10(nx, ny, nt)
    real(4), intent (in), optional :: v10(nx, ny, nt)
    real(4), intent (in), optional :: hgt(nx, ny, nt)
    real(4), intent (in), optional :: tsk(nx, ny, nt)
    real(4), intent (in), optional :: rainc(nx, ny, nt)
    real(4), intent (in), optional :: rainnc(nx, ny, nt)
    real(4), intent (in), optional :: hfx(nx, ny, nt)
    real(4), intent (in), optional :: qfx(nx, ny, nt)
    real(4), intent (in), optional :: lh(nx, ny, nt)
    real(4), intent (in), optional :: sst(nx, ny, nt)
    real(4), intent (in), optional :: slp(nx, ny, nt)
    real(4), intent (in), optional :: maxdbz(nx, ny, nt)
    ! 3D variables
    real(4), intent (in), optional :: u(nx, ny, nz, nt)
    real(4), intent (in), optional :: v(nx, ny, nz, nt)
    real(4), intent (in), optional :: w(nx, ny, nz, nt)
    real(4), intent (in), optional :: qv(nx, ny, nz, nt)
    real(4), intent (in), optional :: qc(nx, ny, nz, nt)
    real(4), intent (in), optional :: qr(nx, ny, nz, nt)
    real(4), intent (in), optional :: qi(nx, ny, nz, nt)
    real(4), intent (in), optional :: qs(nx, ny, nz, nt)
    real(4), intent (in), optional :: qg(nx, ny, nz, nt)
    real(4), intent (in), optional :: hdiabatic(nx, ny, nz, nt)
    real(4), intent (in), optional :: height(nx, ny, nz, nt)
    real(4), intent (in), optional :: tk(nx, ny, nz, nt)
    real(4), intent (in), optional :: theta(nx, ny, nz, nt)
    real(4), intent (in), optional :: rh(nx, ny, nz, nt)
    real(4), intent (in), optional :: dbz(nx, ny, nz, nt)

    integer :: l, k

    ! print 2D array max, min, average values over time
    if (present(xlat) .or. present(xlong) .or. present(q2) .or. &
      present(t2) .or. present(psfc) .or. present(u10) .or. &
      present(v10) .or. present(hgt) .or. present(tsk) .or. &
      present(rainc) .or. present(rainnc) .or. present(hfx) .or. &
      present(qfx) .or. present(lh) .or. present(sst) .or. &
      present(slp) .or. present(maxdbz)) then
      write(*, '(a)') 'DEBUG: 2D variables'
      write(*, '(a)') '       Var_name,  time,  maxval,  minval, average'
    end if
    loop_t_1: do l = 1, nt
      if (present(xlat)) then
        call debug_check_2D_variable_values('xlat', l, &
          maxval(xlat(:, :, l)), minval(xlat(:, :, l)), &
          sum(xlat(:, :, l)) / count(xlat(:, :, l) .ne. nan_val))
      end if
      if (present(xlong)) then
        call debug_check_2D_variable_values('xlong', l, &
          maxval(xlong(:, :, l)), minval(xlong(:, :, l)), &
          sum(xlong(:, :, l)) / count(xlong(:, :, l) .ne. nan_val))
      end if
      if (present(q2)) then
        call debug_check_2D_variable_values('q2', l, &
          maxval(q2(:, :, l)), minval(q2(:, :, l)), &
          sum(q2(:, :, l)) / count(q2(:, :, l) .ne. nan_val))
      end if
      if (present(t2)) then
        call debug_check_2D_variable_values('t2', l, &
          maxval(t2(:, :, l)), minval(t2(:, :, l)), &
          sum(t2(:, :, l)) / count(t2(:, :, l) .ne. nan_val))
      end if
      if (present(psfc)) then
        call debug_check_2D_variable_values('psfc', l, &
          maxval(psfc(:, :, l)), minval(psfc(:, :, l)), &
          sum(psfc(:, :, l)) / count(psfc(:, :, l) .ne. nan_val))
      end if
      if (present(u10)) then
        call debug_check_2D_variable_values('u10', l, &
          maxval(u10(:, :, l)), minval(u10(:, :, l)), &
          sum(u10(:, :, l)) / count(u10(:, :, l) .ne. nan_val))
      end if
      if (present(v10)) then
        call debug_check_2D_variable_values('v10', l, &
          maxval(v10(:, :, l)), minval(v10(:, :, l)), &
          sum(v10(:, :, l)) / count(v10(:, :, l) .ne. nan_val))
      end if
      if (present(hgt)) then
        call debug_check_2D_variable_values('hgt', l, &
          maxval(hgt(:, :, l)), minval(hgt(:, :, l)), &
          sum(hgt(:, :, l)) / count(hgt(:, :, l) .ne. nan_val))
      end if
      if (present(tsk)) then
        call debug_check_2D_variable_values('tsk', l, &
          maxval(tsk(:, :, l)), minval(tsk(:, :, l)), &
          sum(tsk(:, :, l)) / count(tsk(:, :, l) .ne. nan_val))
      end if
      if (present(rainc)) then
        call debug_check_2D_variable_values('rainc', l, &
          maxval(rainc(:, :, l)), minval(rainc(:, :, l)), &
          sum(rainc(:, :, l)) / count(rainc(:, :, l) .ne. nan_val))
      end if
      if (present(rainnc)) then
        call debug_check_2D_variable_values('rainnc', l, &
          maxval(rainnc(:, :, l)), minval(rainnc(:, :, l)), &
          sum(rainnc(:, :, l)) / count(rainnc(:, :, l) .ne. nan_val))
      end if
      if (present(hfx)) then
        call debug_check_2D_variable_values('hfx', l, &
          maxval(hfx(:, :, l)), minval(hfx(:, :, l)), &
          sum(hfx(:, :, l)) / count(hfx(:, :, l) .ne. nan_val))
      end if
      if (present(qfx)) then
        call debug_check_2D_variable_values('qfx', l, &
          maxval(qfx(:, :, l)), minval(qfx(:, :, l)), &
          sum(qfx(:, :, l)) / count(qfx(:, :, l) .ne. nan_val))
      end if
      if (present(lh)) then
        call debug_check_2D_variable_values('lh', l, &
          maxval(lh(:, :, l)), minval(lh(:, :, l)), &
          sum(lh(:, :, l)) / count(lh(:, :, l) .ne. nan_val))
      end if
      if (present(sst)) then
        call debug_check_2D_variable_values('sst', l, &
          maxval(sst(:, :, l)), minval(sst(:, :, l)), &
          sum(sst(:, :, l)) / count(sst(:, :, l) .ne. nan_val))
      end if
      if (present(slp)) then
        call debug_check_2D_variable_values('slp', l, &
          maxval(slp(:, :, l)), minval(slp(:, :, l)), &
          sum(slp(:, :, l)) / count(slp(:, :, l) .ne. nan_val))
      end if
      if (present(maxdbz)) then
        call debug_check_2D_variable_values('maxdbz', l, &
          maxval(maxdbz(:, :, l)), minval(maxdbz(:, :, l)), &
          sum(maxdbz(:, :, l)) / count(maxdbz(:, :, l) .ne. nan_val))
      end if
    end do loop_t_1

    ! print 3D array max, min, average values over height and time
    if (present(u) .or. present(v) .or. present(w) .or. present(qv) .or. &
      present(qc) .or. present(qr) .or. present(qi) .or. present(qs) .or. &
      present(qg) .or. present(hdiabatic) .or. present(height) .or. &
      present(tk) .or. present(theta) .or. present(rh) .or. present(dbz)) then
      write(*, '(a)') 'DEBUG: 3D variables'
      write(*, '(a)') '       Var_name,  time,  height,  maxval,  minval, '//&
        'average'
    end if
    loop_t_2: do l = 1, nt
      loop_z_2: do k = 1, nz
        if (present(u)) then
        call debug_check_3D_variable_values('u', l, k, &
          maxval(u(:, :, k, l)), minval(u(:, :, k, l)), &
          sum(u(:, :, k, l)) / count(u(:, :, k, l) .ne. nan_val))
        end if
        if (present(v)) then
        call debug_check_3D_variable_values('v', l, k, &
          maxval(v(:, :, k, l)), minval(v(:, :, k, l)), &
          sum(v(:, :, k, l)) / count(v(:, :, k, l) .ne. nan_val))
        end if
        if (present(w)) then
        call debug_check_3D_variable_values('w', l, k, &
          maxval(w(:, :, k, l)), minval(w(:, :, k, l)), &
          sum(w(:, :, k, l)) / count(w(:, :, k, l) .ne. nan_val))
        end if
        if (present(qv)) then
        call debug_check_3D_variable_values('qv', l, k, &
          maxval(qv(:, :, k, l)), minval(qv(:, :, k, l)), &
          sum(qv(:, :, k, l)) / count(qv(:, :, k, l) .ne. nan_val))
        end if
        if (present(qc)) then
        call debug_check_3D_variable_values('qc', l, k, &
          maxval(qc(:, :, k, l)), minval(qc(:, :, k, l)), &
          sum(qc(:, :, k, l)) / count(qc(:, :, k, l) .ne. nan_val))
        end if
        if (present(qr)) then
        call debug_check_3D_variable_values('qr', l, k, &
          maxval(qr(:, :, k, l)), minval(qr(:, :, k, l)), &
          sum(qr(:, :, k, l)) / count(qr(:, :, k, l) .ne. nan_val))
        end if
        if (present(qi)) then
        call debug_check_3D_variable_values('qi', l, k, &
          maxval(qi(:, :, k, l)), minval(qi(:, :, k, l)), &
          sum(qi(:, :, k, l)) / count(qi(:, :, k, l) .ne. nan_val))
        end if
        if (present(qs)) then
        call debug_check_3D_variable_values('qs', l, k, &
          maxval(qs(:, :, k, l)), minval(qs(:, :, k, l)), &
          sum(qs(:, :, k, l)) / count(qs(:, :, k, l) .ne. nan_val))
        end if
        if (present(qg)) then
        call debug_check_3D_variable_values('qg', l, k, &
          maxval(qg(:, :, k, l)), minval(qg(:, :, k, l)), &
          sum(qg(:, :, k, l)) / count(qg(:, :, k, l) .ne. nan_val))
        end if
        if (present(hdiabatic)) then
        call debug_check_3D_variable_values('hdiabatic', l, k, &
          maxval(hdiabatic(:, :, k, l)), minval(hdiabatic(:, :, k, l)), &
          sum(hdiabatic(:, :, k, l)) / count(hdiabatic(:, :, k, l) .ne. nan_val))
        end if
        if (present(height)) then
        call debug_check_3D_variable_values('height', l, k, &
          maxval(height(:, :, k, l)), minval(height(:, :, k, l)), &
          sum(height(:, :, k, l)) / count(height(:, :, k, l) .ne. nan_val))
        end if
        if (present(tk)) then
        call debug_check_3D_variable_values('tk', l, k, &
          maxval(tk(:, :, k, l)), minval(tk(:, :, k, l)), &
          sum(tk(:, :, k, l)) / count(tk(:, :, k, l) .ne. nan_val))
        end if
        if (present(theta)) then
        call debug_check_3D_variable_values('theta', l, k, &
          maxval(theta(:, :, k, l)), minval(theta(:, :, k, l)), &
          sum(theta(:, :, k, l)) / count(theta(:, :, k, l) .ne. nan_val))
        end if
        if (present(rh)) then
        call debug_check_3D_variable_values('rh', l, k, &
          maxval(rh(:, :, k, l)), minval(rh(:, :, k, l)), &
          sum(rh(:, :, k, l)) / count(rh(:, :, k, l) .ne. nan_val))
        end if
        if (present(dbz)) then
        call debug_check_3D_variable_values('dbz', l, k, &
          maxval(dbz(:, :, k, l)), minval(dbz(:, :, k, l)), &
          sum(dbz(:, :, k, l)) / count(dbz(:, :, k, l) .ne. nan_val))
        end if
      end do loop_z_2
    end do loop_t_2
  end subroutine debug_check_read_variables

  subroutine debug_tropical_cyclone_center &
    (nz, nt, tcx, tcy, smn)
    implicit none

    integer, intent (in) :: nz, nt
    integer, intent (in) :: tcx(nz, nt), tcy(nz, nt)
    real(4), intent (in) :: smn(nz, nt)

    integer :: k, l

    write(*, '(a)') 'DEBUG: Tropical cyclone center'
    write(*, '(a)') '       time, height, xindex, yindex,  pressure'
    loop_t: do l = 1, nt
      loop_z: do k = 1, nz
        write(*, '(5x, i6, ", ", i6, ", ", i6, ", ", i6, ",", f10.2)') &
          l, k, tcx(k, l), tcy(k, l), smn(k, l)
      end do loop_z
    end do loop_t
  end subroutine debug_tropical_cyclone_center

  subroutine check_out_directory_exists &
    (directory)
    implicit none

    character(len = *), intent (in) :: directory

    logical :: exists

    inquire(file = directory, exist = exists)
    if (.not. exists) then
      write(*, '(a)') 'Creating output directory: '//directory
      call system('mkdir -p '//directory)
    end if
  end subroutine check_out_directory_exists

  subroutine debug_out_file &
    (directory, filename)
    implicit none

    character(len = *), intent (in) :: directory, filename

    write(*, '(a)') 'DEBUG: Output file: '//directory//'/'//filename
  end subroutine debug_out_file

  subroutine error_message_text &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') text
    stop
  end subroutine error_message_text

  subroutine error_message_env_not_set &
    (text)
    implicit none

    character(len = *), intent (in) :: text

    write(*, '(a)') 'ERROR: Environment variable is not set: '//text
    stop
  end subroutine error_message_env_not_set

  subroutine error_message_env_not_number &
    (text, text2)
    implicit none

    character(len = *), intent (in) :: text, text2

    write(*, '(a)') 'ERROR: Environment variable is not a number: '//&
      text//' = '//text2
    stop
  end subroutine error_message_env_not_number

  subroutine error_message_env_unknown &
    (text, text2)
    implicit none

    character(len = *), intent (in) :: text, text2

    write(*, '(a)') 'ERROR: Unknown environment variable setting: '// &
      text//' = '//text2
    stop
  end subroutine error_message_env_unknown

end module module_debug
