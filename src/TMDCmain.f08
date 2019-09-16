program TMDCAnderson
  use constants, only: real12,uline,dline
  use definedTypes, only: basis,settingparam,finegrid,coarsegrid
  use readsettings, only: readin
  use initialisation, only: initGrid,initDO,initHybrid

  implicit none
  
  type(basis)::atomBasis
  type(settingparam)::settings
  type(finegrid),allocatable::kgridFine(:,:,:)
  type(coarsegrid),allocatable::kgridCoarse(:,:,:)

  complex(real12),allocatable::DnDisO(:,:,:,:)
  complex(real12),allocatable::hybOld(:,:,:,:),hybNew(:,:,:,:)
  complex(real12),allocatable::dcGF(:,:,:,:),typGF(:,:,:,:)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! read in settings and initialise variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call readin(atombasis,settings)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! initialise grids, GFO and HYB0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call initGrid(settings,kgridFine,kgridCoarse)
  call initDO(settings,kgridFine,DnDisO)
  call initHybrid(settings%omega2,kgridCoarse%omega2,DnDisO,hybOld)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! start loop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! call loop routine

  ! call test routine; if passed end loop

  ! outputs

end program TMDCAnderson
