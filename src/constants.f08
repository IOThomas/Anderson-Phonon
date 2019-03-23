module constants
  use,intrinsic :: iso_fortran_env
  implicit none
  integer,parameter::real12=real128
  real(real12),parameter::pi=2.0_real12*acos(0.0_real12)
end module constants
