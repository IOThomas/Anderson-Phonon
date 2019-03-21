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


  function coarseDispersion(fineomega2,icellnumber,igridno)
    complex(real12)::coarseDispersion
    !dummyvariables
    real,intent(in)::fineomega2(:,:,:) ! assume we pass a masked temporary array here
    integer,intent(in)::icellnumber,igridno !*total* number of cells grid points

    !routine variables
    real(real12)::ratio

    ratio=real(icellnumber,real12)/real(igridno,real12)

    coarseDispersion=ratio*sum(fineomega2)
    
  end function coarseDispersion

end module dispersions
