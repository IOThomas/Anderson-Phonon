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
    basis%rBasis%Comp=0.0d0
    basis%rBasis%Norm=0.0d0
    basis%kBasis%Comp=0.0d0
    basis%kBasis%Norm=0.0d0
  end subroutine getBasis
    

  subroutine getcoarseGrids(coarseGrid,ncell,unitClength)
    ! dummy variables
    integer::ncell
    type(cellBasis),intent(inout)::coarseGrid
    real(real12)::unitClength

    !routine variables
    integer::ix,iy,iz
    real::lx,ly,lz,px,py,pz
    
    coarseGrid%ncell=ncell
    coarseGrid%nCelltot=ncell*ncell*ncell
    coarseGrid%clusterLength=ncell*unitClength
    
    allocate(coarseGrid%rbasis(ncell,ncell,ncell))
    allocate(coarseGrid%rbasis(ncell,ncell,ncell))

    do ix=1,ncell
       px=2.0*pi*(real(ix-1,real12))/coarseGrid%clusterLength !0 to 2pi
       px=px -pi/unitCLength+ 0.5d0*pi/coarseGrid%clusterLength !-pi to pi,
                                                            !maps to cell centre
       lx=2.0*pi/px
       do iy=1,ncell
          py=2.0*pi*(real(iy-1,real12))/coarseGrid%clusterLength !0 to 2pi
          py=py -pi/unitCLength+ 0.5d0*pi/coarseGrid%clusterLength !-pi to pi,
                                                            !maps to cell centre
          ly=2.0*pi/py
          do iz=1,ncell
             py=2.0*pi*(real(iz-1,real12))/coarseGrid%clusterLength !0 to 2pi
             pz=pz-pi/unitCLength+ 0.5d0*pi/coarseGrid%clusterLength !-pi to pi,
                                                          !maps to cell centre
             
             coarseGrid%rbasis(ix,iy,iz)%Comp(1)=lx
             coarseGrid%rbasis(ix,iy,iz)%Comp(2)=ly
             coarseGrid%rbasis(ix,iy,iz)%Comp(3)=lz
             coarseGrid%rbasis(ix,iy,iz)%Norm=sqrt(lx*lx&
                  +ly*ly+lz*lz)
             
             coarseGrid%kbasis(ix,iy,iz)%Comp(1)=px
             coarseGrid%kbasis(ix,iy,iz)%Comp(2)=py
             coarseGrid%kbasis(ix,iy,iz)%Comp(3)=pz
             coarseGrid%rbasis(ix,iy,iz)%Norm=sqrt(px*px&
                  +py*py+pz*pz)
          enddo
       enddo
    enddo

  end subroutine getcoarseGrids

  subroutine getfineGrids(finegrid,nfinecells,unitClength)
    !dummy variables
    
    
end module initialisation

