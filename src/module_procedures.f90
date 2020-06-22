module module_procedures
  ! Written by P. Cheng
  implicit none

contains

  function is_numeric(string)
    implicit none

    character(len = *), intent (in) :: string
    character(len = 200) :: newstring
    logical :: is_numeric
    real(4) :: x
    integer :: iostats

    newstring = trim(adjustl(string))
    read(newstring, *, iostat = iostats) x
    is_numeric = iostats == 0
  end function is_numeric

  subroutine coordinate &
    (nx, ny, nz, nt, u, v, tcx, tcy, ur, vt)
    ! Rewritten in f90 form by P. Cheng (06/21/2020)
    use module_define_constants
    implicit none

    integer, intent (in) :: nx, ny, nz, nt
    real(4), intent (in) :: u(nx, ny, nz, nt), v(nx, ny, nz, nt)
    integer, intent (in) :: tcx(nz, nt), tcy(nz, nt)
    real(4), intent (out) :: ur(nx, ny, nz, nt), vt(nx, ny, nz, nt)

    integer :: i, j, k, t
    integer :: xc, yc
    real(4) :: sitam, sita

    loop_t: do t = 1, nt
      loop_z: do k = 1, nz
        loop_y: do j = 1, ny
          loop_x: do i = 1, nx
            xc = i - tcx(k, t)
            yc = j - tcy(k, t)

            if (xc .eq. 0) then
              if (yc .ge. 0) then
                sita = .5 * pi
              else
                sita = 1.5 * pi
              end if

            else ! if xc .ne. 0
              sitam = abs(atan(1.*yc/xc))

              if (xc .gt. 0) then
                if (yc .ge. 0) then
                  sita = sitam
                else
                  sita = 2. * pi - sitam
                end if

              else ! if xc .lt. 0
                if (yc .ge. 0) then
                  sita = pi - sitam
                else ! if yc .lt. 0
                  sita = pi + sitam
                end if
              end if
            end if

            if ((u(i, j, k, t) .ne. default) .and. &
                (v(i, j, k, t) .ne. default)) then
              ur(i, j, k, t) = u(i, j, k, t) * cos(sita) + &
                               v(i, j, k, t) * sin(sita)
              vt(i, j, k, t) = v(i, j, k, t) * cos(sita) - &
                               u(i, j, k, t) * sin(sita)
            end if

          end do loop_x
        end do loop_y
      end do loop_z
    end do loop_t
  end subroutine coordinate

  subroutine scinex &
    (nx, ny, gx, gy, scalar, scinto)
    ! This subroutine produces the value scinto of a scalar field at a point
    ! gx, gy by interpolation or extrapolation of the field scala (2-Directi
    ! Bessel interpolation formula). mmin, mmax and nmin, nmax are the boundary
    ! of the grid array.
    ! Rewritten in f90 form by P. Cheng (06/21/2020)
    implicit none

    integer, intent (in) :: nx, ny
    real(4), intent (in) :: gx, gy
    real(4), intent (in) :: scalar(nx, ny)
    real(4), intent (out) :: scinto

    integer :: ii, jj
    real(4) :: mm, nn, a, b, c, d, e, fq, h, p, t1, t2

    ii = int(gx)
    jj = int(gy)
    mm = gx - ii
    nn = gy - jj
    if (mm .lt. 1.e-6) mm = 0.
    if (nn .lt. 1.e-6) nn = 0.
    
    if (gx .lt. nx) then
60    if (gx .ge. 1) then
140     if (gy .ge. ny) then
120       e = gy - ny
          p = scalar(ii, ny) + mm * (scalar(ii+1, ny) - scalar(ii, ny))
          h = scalar(ii, ny-1) + mm * (scalar(ii+1, ny-1) - scalar(ii, ny-1))
          scinto = p + e * (p - h)
        else
          if (gy .ge. 1) then
160         if (gx .lt. nx-1 .and. gx .ge. 2 .and. &
                gy .lt. ny-1 .and. gy .ge. 2) then
180           fq = .25 * mm * (mm - 1)
              a = scalar(ii, jj-1) + &
                  mm * (scalar(ii+1, jj-1) - scalar(ii, jj-1)) + &
                  fq * (scalar(ii+2, jj-1) + scalar(ii-1, jj-1) - &
                        scalar(ii+1, jj-1) - scalar(ii, jj-1))
              b = scalar(ii, jj) + &
                  mm * (scalar(ii+1, jj) - scalar(ii, jj)) + &
                  fq * (scalar(ii+2, jj) + scalar(ii-1, jj) - &
                        scalar(ii+1, jj) - scalar(ii,jj))
              c = scalar(ii, jj+1) + &
                  mm * (scalar(ii+1, jj+1) - scalar(ii, jj+1)) + &
                  fq * (scalar(ii+2, jj+1) + scalar(ii-1, jj+1) - &
                        scalar(ii+1, jj+1) - scalar(ii,jj+1))
              d = scalar(ii, jj+2) + &
                  mm * (scalar(ii+1, jj+2) - scalar(ii, jj+2)) + &
                  fq * (scalar(ii+2, jj+2) + scalar(ii-1, jj+2) - &
                        scalar(ii+1, jj+2) - scalar(ii, jj+2))
              scinto = b + nn * (c - b) + .25 * nn * (nn - 1) * (a + d - b - c)
            else
              p = scalar(ii+1, jj) + nn * (scalar(ii+1, jj+1) - &
                scalar(ii+1, jj))
              h = scalar(ii, jj) + nn * (scalar(ii, jj+1) - scalar(ii,jj))
              scinto = h + mm * (p - h)
            end if
          else
            e = 1 - gy
            p = scalar(ii, 1) + mm * (scalar(ii+1, 1) - scalar(ii, 1))
            h = scalar(ii, 2) + mm * (scalar(ii+1, 2) - scalar(ii, 2))
            scinto = p + e * (p - h)
          end if
        end if
      else ! if gx .lt. 1
        if (gy .lt. ny) then
80        if (gy .ge. 1) then
100         e = 1 - gx
            p = scalar(1, jj) + nn * (scalar(1, jj+1) - scalar(1, jj))
            h = scalar(2, jj) + nn * (scalar(2, jj+1) - scalar(2, jj))
            scinto = p + e * (p - h)
          else ! gy .lt. 1
            e = 1 - gy
            t2 = e * (scalar(1, 1) - scalar(1, 2))
            e = 1 - gx
            t1 = e * (scalar(1, 1) - scalar(2, 1))
            scinto = scalar(1, 1) + t1 + t2
          end if
        else ! if gy .ge. ny
          e = gy - ny
          t2 = e * (scalar(1, ny) - scalar(1, ny-1))
          e = 1 - gx
          t1 = e * (scalar(1, ny) - scalar(2,ny))
          scinto = scalar(1, ny) + t1 + t2
        end if
      end if
    else ! if gx .ge. nx
      if (gy .lt. ny) then
20      if (gy .ge. 1) then
40        p = scalar(nx, jj) + nn * (scalar(nx, jj+1) - scalar(nx, jj))
          h = scalar(nx-1, jj) + nn * (scalar(nx-1, jj+1) - scalar(nx-1, jj))
          e = gx - nx
          scinto = p + e * (p - h)
        else ! if gy .lt. 1
          e = gx - nx
          t1 = e * (scalar(nx, 1) - scalar(nx-1, 1))
          e = 1 - gy
          t2 = e * (scalar(nx, 1) - scalar(nx, 2))
          scinto = scalar(nx, 1) + t1 + t2
        end if
      else ! if gy .ge. ny
        e = gx - nx
        t1 = e * (scalar(nx, ny) - scalar(nx-1, ny))
        e = gy - ny
        t2 = e * (scalar(nx, ny) - scalar(nx, ny-1))
        scinto = scalar(nx, ny) + t1 + t2
      end if
    end if
  end subroutine scinex

  subroutine symmetric &
    (nx, ny, nz, nt, nr, tcx, tcy, vt, vtb)
    ! This subroutine is used to partition the symetric field 
    ! Rewritten in f90 form by P. Cheng (06/21/2020)
    use module_define_constants
    implicit none

    integer, intent (in) :: nx, ny, nz, nt, nr
    integer, intent (in) :: tcx(nz, nt), tcy(nz, nt)
    real(4), intent (in) :: vt(nx, ny, nz, nt)
    real(4), intent (out) :: vtb(nr, nz, nt)

    real(4) :: vt2(nx, ny)
    real(4) :: sita, sum, gx, gy, scinto
    integer :: total
    integer :: i, j, k, t, n, s

    loop_t: do t = 1, nt
      loop_z: do k = 1, nz
        loop_y: do j = 1, ny
          loop_x: do i = 1, nx
            vt2(i, j) = vt(i, j, k, t)
          end do loop_x
        end do loop_y

        vtb(1, k, t) = 0.

        loop_r: do n = 2, nr
          sita = 0.
          sum = 0.
          total = 0

          loop_s: do s = 1, 360
            sita = float(s) * degtorad
            gx = cos(sita) * (n-1) + tcx(k, t)
            gy = sin(sita) * (n-1) + tcy(k, t)

            if (gx .lt. 2. .or. gx .gt. nx - 1. .or. &
                gy .lt. 2. .or. gy .gt. ny - 1.) then
              cycle ! go to next loop_s
            else
              call scinex(nx, ny, gx, gy, vt2, scinto)
              if (scinto .ne. default .and. abs(scinto) .lt. 1.e4) then
                sum = sum + scinto
                total = total + 1
              end if
            end if
          end do loop_s

          if (total .ne. 0) then
            vtb(n, k, t) = sum / total
          else
            vtb(n, k, t) = 0.
          end if
        end do loop_r
      end do loop_z
    end do loop_t
  end subroutine symmetric

  subroutine find_tropical_cyclone_center &
    (nx, ny, nz, nt, pressure, tcx, tcy, smn)
    implicit none

    integer, intent (in) :: nx, ny, nz, nt
    real(4), intent (in) :: pressure(nx, ny, nz, nt)
    integer, intent (out) :: tcx(nz, nt), tcy(nz, nt)
    real(4), intent (out) :: smn(nz, nt)

    integer :: i, j, k, l

    tcx = 0
    tcy = 0
    smn = 0.
    
    loop_t: do l = 1, nt
      loop_z: do k = 1, nz
      smn(k, l) = pressure(120, 120, k, l)
        loop_y: do j = 1, ny
          loop_x: do i = 1, nx
            if (pressure(i, j, k, l) .le. smn(k, l)) then
              smn(k, l) = pressure(i, j, k, l)
              tcy(k, l) = j
              tcx(k, l) = i
            end if
          end do loop_x
        end do loop_y
      end do loop_z
    end do loop_t
  end subroutine find_tropical_cyclone_center

  subroutine calculate_r17 &
    (nz, nt, nr, vtb, r17)
    implicit none

    integer, intent (in) :: nz, nt, nr
    real(4), intent (in) :: vtb(nr, nz, nt)
    integer, intent (out) :: r17(nt)

    integer :: t, k, r

    r17(:) = 0
    loop_t: do t = 1, nt
      loop_z: do k = 1, nz
        loop_r: do r = 1, nr-1
          if ((vtb(r, k, t) .gt. 17) .and. (vtb(r+1, k, t) .lt. 17)) then
            r17(t) = r * 2
          end if
        end do loop_r
      end do loop_z
    end do loop_t
  end subroutine calculate_r17

end module module_procedures
