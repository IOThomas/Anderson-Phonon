program TMDCAnderson
  use constants, only: real12,uline,dline
  use definedTypes, only: basis,settingparam,finegrid,coarsegrid,storedparam,&
        greensfunc
  use readsettings, only: readin
  use initialisation, only: initGrid,initDzero,initHybrid

  implicit none
  
  type(basis)::atomBasis
  type(settingparam)::settings
  type(storedparam)::stored
  type(finegrid),allocatable::kgridFine(:,:,:)
  type(coarsegrid),allocatable::kgridCoarse(:,:,:)


  integer                     :: ierr
  type(greensfunc),allocatable :: Dzero(:,:,:,:)
  type(greensfunc),allocatable :: GAMMAold(:,:,:,:),GAMMAnew(:,:,:,:)
 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! read in settings and initialise variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call readin(atombasis,settings)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! initialise grids
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call initGrid(settings,kgridFine,kgridCoarse,ierr)
  write(*, *) "Grid initialised"
  if (ierr.eq.1) stop "Number of fine or coarse points is less than 1. Halting."
  if (ierr.eq.2) stop "Number of fine or coarse points is even. Halting."
  if (ierr.eq.3) stop "Input arrays already allocated. Halting."
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! initialise Dzero and Hybridisation arrays
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call initDzero(settings,kgridFine,stored, Dzero, ierr)
  write(*, *) "Fine grid non-interacting GF initialised"
  if (ierr.eq.1) stop "Number of fine k or omega points less than 1. Halting."
  if (ierr.eq.2) stop "Negative omega range. Halting."
  if (ierr.eq.3) stop "Input array not allocated. Halting."
  if (ierr.eq.4) stop "Output array already allocated. Halting."
  
  call initHybrid(stored,kgridCoarse,Dzero,GAMMAold,ierr)
  write(*, *) "Coarse grid hybridisation function initialised"
  if (ierr.eq.1) stop "Omega point spacing zero or less. Halting."
  if (ierr.eq.2) stop "Input array(s) not allocated. Halting."
  if (ierr.eq.3) stop "Output array already allocated. Halting."
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! start loop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! call loop routine

  ! call test routine; if passed end loop

  ! outputs

end program TMDCAnderson
