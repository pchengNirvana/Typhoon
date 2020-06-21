module module_define_variables
  ! Written by P. Cheng
  implicit none

  ! for input and output
  integer, parameter :: strlen = 200
  character(len = strlen) :: env, env_name, sdummy
  character(len = strlen) :: dat_dir, dat_file
  character(len = strlen) :: ctl_dir, ctl_file
  character(len = strlen) :: nc_dir,  nc_file
  character(len = strlen) :: out_dir, out_file
  integer :: lun
  logical :: exists, debug, use_netcdf

  ! dimensions and indices
  integer :: nx, ny, nz, nt, nr, zmax, tmax
  integer :: i, j, k, l, t, r

  ! .dat 2D variables variables
  real(4), allocatable :: xlat(:, :, :)
  real(4), allocatable :: xlong(:, :, :)
  real(4), allocatable :: q2(:, :, :)
  real(4), allocatable :: t2(:, :, :)
  real(4), allocatable :: psfc(:, :, :)
  real(4), allocatable :: u10(:, :, :)
  real(4), allocatable :: v10(:, :, :)
  real(4), allocatable :: hgt(:, :, :)
  real(4), allocatable :: tsk(:, :, :)
  real(4), allocatable :: rainc(:, :, :)
  real(4), allocatable :: rainnc(:, :, :)
  real(4), allocatable :: hfx(:, :, :)
  real(4), allocatable :: qfx(:, :, :)
  real(4), allocatable :: lh(:, :, :)
  real(4), allocatable :: sst(:, :, :)
  real(4), allocatable :: slp(:, :, :)
  real(4), allocatable :: maxdbz(:, :, :)

  ! .dat 3D variables
  real(4), allocatable :: u(:, :, :, :)
  real(4), allocatable :: v(:, :, :, :)
  real(4), allocatable :: w(:, :, :, :)
  real(4), allocatable :: qv(:, :, :, :)
  real(4), allocatable :: qc(:, :, :, :)
  real(4), allocatable :: qr(:, :, :, :)
  real(4), allocatable :: qi(:, :, :, :)
  real(4), allocatable :: qs(:, :, :, :)
  real(4), allocatable :: qg(:, :, :, :)
  real(4), allocatable :: hdiabatic(:, :, :, :)
  real(4), allocatable :: height(:, :, :, :)
  real(4), allocatable :: tk(:, :, :, :)
  real(4), allocatable :: theta(:, :, :, :)
  real(4), allocatable :: rh(:, :, :, :)
  real(4), allocatable :: dbz(:, :, :, :)

  ! derived variables
!  real(4), allocatable :: ur(:, :, :, :)
!  real(4), allocatable :: vt(:, :, :, :)
!  real(4), allocatable :: vtb(:, :, :)
!  real(4), allocatable :: urb(:, :, :)
!  real(4), allocatable :: wb(:, :, :)
!  real(4), allocatable :: hb(:, :, :)
!  real(4), allocatable :: tcx(:, :)
!  real(4), allocatable :: tcy(:, :)
!  real(4), allocatable :: vtb10(:, :)
!  real(4), allocatable :: smn(:, :)
!  real(4), allocatable :: r17(:)

end module module_define_variables
