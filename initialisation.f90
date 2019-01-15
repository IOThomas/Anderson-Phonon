module initialisation
  use constants
  use definedTypes
  implicit none

contains

  subroutine getBasis(basis)
    !dummy variables
    type(atomicBasis),intent(inout)::basis

    ! here we fix the basis to be a monoatomic square cell
    basis%nAtom=1
    basis%unitClength=1.0
    allocate(basis%rBasis(1),basis%kBasis(1))
    basis%rBasis=0.0d0
    basis%kBasis=0.0d0
  end subroutine getBasis
    

  subroutine getGrids(coarseGrid,ncell)!,unitClength)
    ! dummy variables
    integer::ncell
    type(cellBasis),intent(inout)::coarseGrid

    !routine variables
    integer::ix,iy,iz
    real::lx,ly,lz
    
    coarseGrid%ncell=ncell
    coarseGrid%nCelltot=ncell*ncell*ncell
    coarseGrid%clusterLength=ncell*1.0d0!unitClength
    
    allocate(coarseGrid%rbasis(ncell,ncell,ncell))
    allocate(coarseGrid%rbasis(ncell,ncell,ncell))

    do ix=1,ncell
       lx=real(ix-1,real12)
       do iy=1,ncell
          ly=real(iy-1,real12)
          do iz=1,ncell
             lz=real(iz-1,real12)
             coarseGrid%rbasis(ix,iy,iz)%xComp=lx
             coarseGrid%rbasis(ix,iy,iz)%yComp=ly
             coarseGrid%rbasis(ix,iy,iz)%zComp=lz

             coarseGrid%kbasis(ix,iy,iz)%xComp=lx*pi
             coarseGrid%kbasis(ix,iy,iz)%yComp=ly*pi
             coarseGrid%kbasis(ix,iy,iz)%zComp=lz*pi
          enddo
       enddo
    enddo

  end subroutine getGrids
end module initialisation
