module gf_test_routines
  ! contains useful functions and subroutines for gf_fourier unit tests
  use constants
  implicit none
contains
  
  complex(real12) function initial_function(param, ix, iy, iz)
    ! function for FFT test initialisation
    real(real12), intent(in):: param
    integer, intent(in) :: ix, iy, iz

    initial_function =  cmplx(param**real(ix + iy + iz - 3, real12), &
         & zero, real12)
  end function initial_function

  complex(real12) function intermediate_function(param, ipos, length, orig_fun)
    ! function for post-FFT test values
    real(real12), intent(in):: param, length
    integer, intent(in) :: ipos
    complex(real12), intent(in) :: orig_fun

    complex(real12) :: phase, denom

    phase = cmplx(zero, -two*pi*real((ipos -1),real12)/length,real12)
    if ((real(orig_fun,real12).eq.real(exp(-phase),real12))&
         .and.(aimag(orig_fun).eq.aimag(exp(-phase))).and.(param.eq.one)) then
       intermediate_function = cmplx(length,zero,real12)
    else
       denom = cmplx(one,zero,real12) - cmplx(param, zero,real12)*exp(phase)
       intermediate_function = cmplx(one-param**(length), zero, real12)/denom
    endif
  end function intermediate_function

end module gf_test_routines

   
