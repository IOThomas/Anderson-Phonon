module dispersions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! contains definitions of the phonon dispersions
! currently only simple acoustic branch for testing
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  use constants, only: real12
  implicit none
  private
  public finedispersion, coarseDispersion

contains
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental pure function fineDispersion(kx,ky,kz)
    complex(real12)::fineDispersion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! defines the momentum at each point using an acoustic dispersion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !dummy variables
    real(real12),intent(in)::kx,ky,kz

    complex(real12)::test
    
    fineDispersion=cmplx(sin(kx/2.0_real12)*sin(kx/2.0_real12)&
         +sin(ky/2.0_real12)*sin(ky/2.0_real12)&
         +sin(kz/2.0_real12)*sin(kz/2.0_real12),0.0_real12)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end function fineDispersion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function coarseDispersion(fineomega2,icellnumber,igridno)
    complex(real12)::coarseDispersion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! sums and normalises fine grid momenta from an input array
! fineomega2 is *masked* to set non relevent contributions to zero
! icell number, igrid number are the *total* numbers of cells in each grid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !dummyvariables
    complex(real12),intent(in)::fineomega2(:,:,:)
    integer,intent(in)::icellnumber,igridno 

    !routine variables
    real(real12)::ratio

    ratio=real(icellnumber,real12)/real(igridno,real12)

    coarseDispersion=ratio*sum(fineomega2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end function coarseDispersion
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module dispersions
