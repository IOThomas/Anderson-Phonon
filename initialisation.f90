module initialisation
  use constants
  use definedTypes
  implicit none

contains

  subroutine initGrid(settings,kgridFine,kgridCoarse)
    type(settingparam),intent(in)::settings
    type(finegrid),allocatable,intent(inout)::kgridFine(:,:,:)
    type(coarsegrid),allocatable,intent(inout)::kgridCoarse(:,:,:)

    !routine variables
    integer::temp(3)

    !allocate fine grid and coarse grid sizes
    temp=settings%nfpoint
    allocate(kgridFine(temp(1),temp(2),temp(3)))
    temp=settings%ncell
    allocate(kgridCoarse(temp(1),temp(2),temp(3)))

    !allocate k grid values

    !allocate the coarse map points

    !calculate frequency squared for both grids

  end subroutine initGrid

  subroutine initGF()

    !generate DO for fine grid points

    !map to coarse grid

  end subroutine initGF

  subroutine initHybrid()

    ! generate initial hybridisation

  end subroutine initHybrid

    
end module initialisation
