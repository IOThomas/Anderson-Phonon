program TMDCAnderson

  use constants
  use derivedTypes
  use readsettings
  use initialisation
  use dynamicalLoop

  implicit none
  
  type(basis)::atomBasis
  type(settingparam)::settings
  type(finegrid),allocatable::kgridFine(:,:,;)
  type(coarsegrid),allocatable::kgridCoarse(:,:,:)
  
  complex(real12),allocatable::hybOld(:,:,:,:),hybNew(:,:,:,:)
  complex(real12),allocatable::dcGF(:,:,:,:),typGF(:,:,:,:)
  
  ! read in settings and initialise variables
  call readin(atombasis,settings,kgridFine,kgridCoarse)
  

  ! initialise grids, GFO and HYB0
  call 

  ! start loop

  ! call loop routine

  ! call test routine; if passed end loop

  ! outputs

end program TMDCAnderson
