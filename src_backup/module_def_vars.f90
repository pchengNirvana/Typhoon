module module_def_vars
  implicit none

  ! control variables
  integer, parameter :: strlen = 200
  character(len = strlen) :: env, sdummy
  character(len = strlen) :: data_dir
  character(len = strlen) :: typhoon_id
  character(len = *), parameter :: slash = '/'
  character(len = 2) :: domain
  logical :: exists

  ! original variables
  real(4), allocatable, dimension(:, :, :, :) :: u, v, p, w, h
  real(4), allocatable, dimension(:, :, :, :) :: ur, vt
  real(4), allocatable, dimension(:, :, :, :) :: u10, v10, slp
  real(4), allocatable, dimension(:, :, :)    :: vtb, urb, wb, hb
  real(4), allocatable, dimension(:, :)       :: tcx, tcy, vtb10
  integer :: nx, ny, nz, nt, nr
  integer :: i, j, k, t, r

  integer :: uvrec, vtbrec, file_recl
  real(4) :: fff
  real(4), parameter :: default = -9.9900000E+08

  ! reassigned allocatable array
  real, allocatable, dimension(:) :: smn, r17


  character(len = 2) :: domain
  integer :: ncid
  integer, allocatable :: dimid(:), varid(:)
  integer :: it, interval_h
  character(len = 10) :: wrf_timestring

  ! for writing outputs
  integer :: lun
  character(len = strlen) :: out_dir, out_file, out_file_suffix

end module module_def_vars
