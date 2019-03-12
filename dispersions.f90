module dispersions
  ! contains definitions of the phonon dispersions
  ! currently only simple acoustic
  use constants
  implicit none

contains

  function fineDispersion(kx,ky,kz)
    real(real12)::fineDispersion
    !dummy variables
    real(real12),intent(in)::kx,ky,kz

    fineDispersion=cos(kx/2.0d0)*cos(kx/2.0d0)+cos(ky/2.0d0)*cos(ky/2.0d0)&
         +cos(kz/2.0d0)*cos(kz/2.0d0)
  end function fineDispersion


  function coarseDispersion
  end function coarseDispersion

end module dispersions
