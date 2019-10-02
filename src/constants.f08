module constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use,intrinsic :: iso_fortran_env
  implicit none
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! numerical constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer, parameter, public      :: real12 = real128
  real(real12), parameter, public :: zero = 0.0_real12
  real(real12), parameter, public :: half = 0.5_real12
  real(real12), parameter, public :: one = 1.0_real12
  real(real12), parameter, public :: two = 2.0_real12
  real(real12), parameter, public :: three = 3.0_real12
  real(real12), parameter, public :: pi = two*acos(zero)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! character/other constants
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !uline, dline used as part of output formatting
  character(len=79), parameter, public :: uline = repeat("_", 79)
  character(len=79), parameter, public :: dline = repeat("-", 79) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module constants
