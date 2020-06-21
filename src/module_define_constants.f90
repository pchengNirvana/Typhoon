module module_define_constants
  implicit none

  ! constants
  character(len = *), parameter :: slash = '/'
  real(4), parameter :: default = -9.9900000e+08
  real(4), parameter :: pi = 4.0*atan(1.0) ! 3.14159265358979323
  real(4), parameter :: degtorad = pi / 180.
  real(4), parameter :: radtodeg = 180. / pi


end module module_define_constants
