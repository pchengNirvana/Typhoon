module module_define_variables
  implicit none

  ! control variables
  integer, parameter :: strlen = 200
  character(len = strlen) :: env, env_name, sdummy
  character(len = strlen) :: dat_dir, dat_file
  character(len = strlen) :: in_file_suffix
  character(len = strlen) :: typhoon_id
  character(len = 2) :: domain
  logical :: exists, debug

  ! original variables
  real(4), allocatable, dimension(:, :, :, :) :: u, v, p, w, h
  real(4), allocatable, dimension(:, :, :, :) :: ur, vt
  real(4), allocatable, dimension(:, :, :, :) :: u10, v10, slp
  real(4), allocatable, dimension(:, :, :)    :: vtb, urb, wb, hb
  real(4), allocatable, dimension(:, :)       :: tcx, tcy, vtb10
  integer :: nx, ny, nz, nt, nr
  integer :: i, j, k, t
  integer :: uvrec, vtbrec, file_recl
  real(4) :: fff

  ! reassigned allocatable array
  real, allocatable :: smn(:, :), r17(:)

  ! for writing outputs
  integer :: lun
  character(len = strlen) :: out_dir, out_file, out_file_suffix

end module module_define_variables
