module constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Constants used throughout program.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use, intrinsic :: iso_fortran_env
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! numerical constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer, parameter, public      :: real12 = real128
  !# numerical precision
  real(real12), parameter, public :: zero = 0.0_real12
  real(real12), parameter, public :: half = 0.5_real12
  real(real12), parameter, public :: one = 1.0_real12
  real(real12), parameter, public :: two = 2.0_real12
  real(real12), parameter, public :: three = 3.0_real12
  real(real12), parameter, public :: pi = two*acos(zero)
  real(real12), parameter, public :: cmplx_zero = (zero, zero)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! tolerances 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real(real12), parameter, public :: scale_tol = 1.0e4_real12
  !# allows easy scaling of tolerance (set at approx FFT test accuracy)
  real(real12), parameter, public :: tolerance = scale_tol*epsilon(one)
  !# typical numerical tolerance 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! character/other constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !uline, dline used as part of output formatting
  character(len=79), parameter, public :: uline = repeat("_", 79)
  !# underlining for output formatting
  character(len=79), parameter, public :: dline = repeat("-", 79)
  !# row of dashes for output formatting
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module constants
