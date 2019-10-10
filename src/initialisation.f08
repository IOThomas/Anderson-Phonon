module initialisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! contains initialisation subroutines for the calculation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use constants, only: real12, pi, half, two
  use dispersions, only: finedispersion, coarseDispersion
  use definedtypes, only: settingparam, finegrid, coarsegrid
  implicit none
  private
  public initGrid, initDO, initHybrid

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initGrid(settings, kgridFine, kgridCoarse, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialises coarse and fine momentum grids for the calculation
! Note that the formula for assigning the kpoint values is a little non-obvious
! and only works for *even* values of Ncell and Nfpoints
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Error codes: ierr=0: Routine executed fine
!              ierr=1: Ncell or Nfpoints are negative or zero
!              ierr=2: Ncell or Nfpoints are odd
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(settingparam), intent(in)               :: settings
    type(finegrid), allocatable, intent(inout)   :: kgridFine(:,:,:)
    type(coarsegrid), allocatable, intent(inout) :: kgridCoarse(:,:,:)
    integer                                      :: ierr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer                      :: itest1(3)
    integer                      :: itest2(3)
    integer                      :: icond(3)  
    integer                      :: itemp(3)
    integer                      :: ix, iy, iz
    integer                      :: iNcell, iNpoint
    real(real12)                 :: kx, ky, kz
    real(real12)                 :: length(3)
    real(real12)                 :: lowLim(3), upperLim(3)
    complex(real12), allocatable :: tempArray(:,:,:)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! check number of grid points, return uninitialised if unusable
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    itest1=settings%nfpoints
    itest2=settings%ncell
    icond=1
    if (any(itest1.lt.icond).or.any(itest2.lt.icond)) then
       ierr=1
       return
    elseif (any(mod(itest1,2).eq.icond).or.any(mod(itest2,2).eq.icond)) then
       ierr=2
       return
    else
       ierr=0
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! allocate fine grid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    itemp = settings%nfpoints
    length = two * pi / real(itemp, real12)
    allocate(kgridFine(itemp(1), itemp(2), itemp(3)))
    allocate(tempArray(itemp(1), itemp(2), itemp(3)))
    do iz = 1, itemp(3)
       kz=squaregridmom(iz,itemp(3),length(3))
       do iy = 1, itemp(2)
          ky=squaregridmom(iy,itemp(2),length(2))
          do ix =1, itemp(1)
             kx=squaregridmom(ix,itemp(1),length(1))
             
             kgridFine(ix, iy, iz)%kx = kx
             kgridFine(ix, iy, iz)%ky = ky
             kgridFine(ix, iy, iz)%kz = kz
             kgridFine(ix, iy, iz)%norm = sqrt(kx * kx + ky * ky + kz * kz)
             kgridFine(ix,iy,iz)%omega2 = fineDispersion(kx, ky, kz)
          enddo
       enddo
    enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
! allocate coarsegrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    itemp = settings%ncell
    allocate(kgridCoarse(itemp(1), itemp(2), itemp(3)))
    length = two * pi / real(itemp, real12)
    do iz = 1, itemp(3)
       kz=squaregridmom(iz,itemp(3),length(3))
       do iy = 1, itemp(2)
          ky=squaregridmom(iy,itemp(2),length(2))
          do ix = 1, itemp(1)
             kx=squaregridmom(ix,itemp(1),length(1))
             
             kgridCoarse(ix, iy, iz)%kx = kx
             kgridCoarse(ix, iy, iz)%ky = ky
             kgridCoarse(ix, iy, iz)%kz = kz
             kgridCoarse(ix, iy, iz)%norm = sqrt(kx * kx + ky * ky + kz * kz)
             kgridCoarse(ix, iy, iz)%label = ix + (iy -1 ) * itemp(1)&
                  + (iz - 1) * itemp(1) * itemp(2)
          enddo
       enddo
    enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! allocate the coarse map points and the associated coarse momentum
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    iNcell = settings%ncell(1) * settings%ncell(2) * settings%ncell(2)
    iNpoint = settings%nfpoints(1) * settings%nfpoints(2) * settings%nfpoints(2)
    do ix = 1, itemp(1)
       do iy = 1, itemp(2)
          do iz = 1, itemp(3)
             lowLim(1) = kgridCoarse(ix, iy, iz)%kx - half * length(1)
             upperLim(1) = kgridCoarse(ix, iy, iz)%kx + half * length(1)

             lowLim(2) = kgridCoarse(ix, iy, iz)%ky - half * length(2)
             upperLim(2) = kgridCoarse(ix, iy, iz)%ky + half *length(2)
          
             lowLim(3) = kgridCoarse(ix, iy, iz)%kz - half * length(3)
             upperLim(3) = kgridCoarse(ix, iy, iz)%kz + half * length(3)

             tempArray = 0.0d0
             where ((kgridFine%kx >= lowLim(1)).and.&
                  (kgridFine%kx < upperLim(1)).and. &
                  (kgridFine%ky >= lowLim(2)).and.&
                  (kgridFine%ky < upperLim(2)).and. &
                  (kgridFine%kz >= lowLim(3)).and.&
                  (kgridFine%kz < upperLim(3)))

                kgridFine%coarsemap = kgridCoarse(ix, iy, iz)%label
                tempArray = kgridFine%omega2
             end where
             kgridCoarse(ix, iy, iz)%omega2 = &
                  coarseDispersion(tempArray, iNcell, iNpoint)           
          enddo
       enddo
    enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! output grids
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    open(unit=10, file='finekgrid.out', status='unknown')
    write(10,*) '# fine kpoint grid'
    write(10,*) 'ix, iy, iz, kx, ky, kz, |k|, Re(omega**2),  Im(omega**2), <map to this coarse grid point>'
    itemp = settings%nfpoints
    do ix = 1, itemp(1)
       do iy = 1, itemp(2)
          do iz = 1, itemp(3)
             write(10, fmt=800) ix, iy, iz, kgridFine(ix, iy, iz)%kx, &
                  kgridFine(ix, iy, iz)%ky, kgridFine(ix, iy, iz)%kz,&
                  kgridFine(ix, iy, iz)%norm,&
                  kgridFine(ix, iy, iz)%omega2, kgridFine(ix, iy, iz)%coarsemap
          enddo
       enddo
    enddo
    close(10)
    
    open(unit=10, file='coarsekgrid.out', status='unknown')
    write(10,*) '# coarse kpoint grid'
    write(10,*) 'ix, iy, iz, kx, ky, kz, |k|, Re(omega**2), Im(omega**2), cell label'
    itemp = settings%ncell
    do ix = 1, itemp(1)
       do iy = 1, itemp(2)
          do iz= 1, itemp(3)
             write(10, fmt=800) ix, iy, iz, kgridCoarse(ix, iy, iz)%kx, &
                  kgridCoarse(ix, iy, iz)%ky, kgridCoarse(ix, iy, iz)%kz,&
                  kgridCoarse(ix, iy, iz)%norm,&
                  kgridCoarse(ix, iy, iz)%omega2,kgridCoarse(ix, iy, iz)%label
          enddo
       enddo
    enddo
    close(10)

    deallocate(tempArray)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!!!!
! FORMAT BLOCKS !
!!!!!!!!!!!!!!!!!
800 format(3i5, 4es25.15, 2es25.15, i5)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  contains
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    pure function squaregridmom(ipoint,itotal,tlength)
      real(real12) :: squaregridmom
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculates the momentum at a given point in a square grid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer, intent(in) :: ipoint
      integer, intent(in) :: itotal
      real(real12), intent(in) :: tlength
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      squaregridmom = (real(ipoint, real12)&
           - half * real( 1 + (itotal/ 2), real12)) * tlength
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end function squaregridmom
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  end subroutine initGrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initDO(settings,kgridFine,DnDisO)
    type(settingparam),intent(inout)::settings
    type(finegrid),intent(in)::kgridFine(:,:,:)
    complex(real12),allocatable,intent(out)::DnDisO(:,:,:,:)

    ! routine variables
    integer::i,j,k,l,nmax, nfmax,nfpoints(3), ncell(3)
    complex(real12)::omega_diff
    complex(real12),allocatable::tempArray(:,:,:,:),tempArray1(:,:)
    complex(real12),allocatable::tempArray2(:,:)

    nfpoints=settings%nfpoints
    ncell=settings%ncell

    !generate omega2 differences
    allocate(settings%omega2diff(nfpoints(1),nfpoints(2),nfpoints(3),&
         settings%nomega))
    allocate(settings%omega2(settings%nomega))
    omega_diff=(settings%omegaMax-settings%omegaMin)/&
         (real(settings%nomega-1,real12))
    forall(i=1:settings%nomega)
       settings%omega2(i)=(real((i-1),real12)*omega_diff)**2
    end forall
    forall(i=1:nfpoints(1),j=1:nfpoints(2),k=1:nfpoints(3),l=1:settings%nomega)
       settings%omega2diff(i,j,k,l)=(real((l-1),real12)*omega_diff)**2 &
            -(kgridFine(i,j,k)%omega2)**2
    endforall
    allocate(settings%pointsmap(nfpoints(1),nfpoints(2),nfpoints(3),&
         settings%nomega))
    forall (i=1:settings%nomega)
       settings%pointsmap(1:nfpoints(1),1:nfpoints(2),1:nfpoints(3),i)=&
            kgridFine%coarseMap
    endforall
    
    !generate DO for fine grid points
    allocate(tempArray(nfpoints(1),nfpoints(2),nfpoints(3),&
         settings%nomega))
    allocate(DnDisO(ncell(1),ncell(2),ncell(3),settings%nomega))
    
    !map to coarse grid
    tempArray=0.0_real12
    nmax=ncell(1)*ncell(2)*ncell(3)
    nfmax=nfpoints(1)*nfpoints(2)*nfpoints(3)
    !redefine what follows as a function maybe? (will need to do again...)
    allocate(tempArray1(nmax,settings%nomega))
    allocate(tempArray2(nfmax,settings%nomega))
    tempArray1=reshape(DnDisO, [nmax,settings%nomega])
    do i=1,nmax
       where(settings%pointsmap==i)
          ! will need to rework for matrix case?
          tempArray=1.0_real12/settings%omega2diff
       end where
       tempArray2=reshape(tempArray2,[nmax,settings%nomega])
       forall(j=1:settings%nomega)
          tempArray1(i,j)=real(nmax,real12)*sum(tempArray2(1:nmax,j))&
               /real(nfmax,real12)
       end forall
    enddo
    DnDisO=reshape(tempArray1,[ncell(1),ncell(2),ncell(3),settings%nomega])

    deallocate(tempArray,tempArray1,tempArray2)
  end subroutine initDO

  subroutine initHybrid(w2raw,coarsew2,D0,startHybrid)
    complex(real12),intent(in)::w2raw(:),coarsew2(:,:,:)
    complex(real12),intent(in)::D0(:,:,:,:)
    complex(real12),allocatable,intent(out)::startHybrid(:,:,:,:)

    !routine variables
    integer:: nx,ny,nz,nw2
    integer:: i,j,k,l

    nx=size(D0,1)
    ny=size(D0,2)
    nz=size(D0,3)
    nw2=size(D0,4)
    
    allocate(startHybrid(nx,ny,nz,nw2))
    !may have to rewrite for matrix form...
    forall (i=1:nx,j=1:ny,k=1:nz,l=1:nw2)
       startHybrid(i,j,k,l)=w2raw(l)-coarsew2(i,j,k)-1.0_real12/D0(i,j,k,l)
    end forall
   

  end subroutine initHybrid

    
end module initialisation
