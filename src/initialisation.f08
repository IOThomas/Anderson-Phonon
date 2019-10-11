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
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
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
    real(real12)                 :: lowLim(3), upperLim(3), ktemp(3)
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
             ktemp(1) = kgridCoarse(ix, iy, iz)%kx
             ktemp(2) = kgridCoarse(ix, iy, iz)%ky
             ktemp(3) = kgridCoarse(ix, iy, iz)%kz
             
             lowLim= ktemp - half * length
             upperLim = ktemp + half * length

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
!------------------------------------------------------------------------------ 
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
!------------------------------------------------------------------------------ 
  end subroutine initGrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initDO(settings, kgridFine, stored, Dzero, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(settingparam), intent(inout)          :: settings
    type(finegrid), intent(in)                 :: kgridFine(:,:,:)
    type(storedparam), intent(inout)           :: stored
    type(greensfunc),allocatable,intent(inout) :: Dzero(:,:,:,:)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer         :: i, j, k, l
    integer         :: nfpoints(3)
    integer         :: nomega,
    integer         :: icond(3)
    complex(real12) :: omega_diff
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! get number of fine grid and omega points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    nfpoints = settings%nfpoints
    nomega = settings%nomega
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! check input for errors
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    icond = 1
    if (any(nfpoints.lt.icond(3)).or.(nomega.lt.1)) then
       ierr=1
       return
    elseif (omegaMax.lt.zero) then
       ierr=2
       return
    else
       ierr=0
    end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! allocate and calculate values of Dzero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    omega_diff = (settings%omegaMax - settings%omegaMin) / real(nomega, real12)
    stored%omega_diff=omega_diff
    
    allocate(Dzero(nfpoints(1), nfpoints(2), nfpoints(3), nomega))
    
    forall (i = 1:nfpoints(1), j = 1:nfpoints(2), k = 1:nfpoints(3), &
         l = 1:nomega)
       Dzero(i, j, k, l)%GF = (real(l**2,real12)*omega_diff*omega_diff, zero)&
            - kfinegrid(i, j, k)%omega2
       Dzero(i, j, k, l)%map = kfinegrid(i, j, k)%coarseMap
    end forall
    Dzero%GF = one / Dzero%GF ! assumes diagonal matrix!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine initDO
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initHybrid(stored, kgridCoarse, Dzero, Gzero )
    type(storedparam), intent(in)   :: stored
    type(coarsegrid), intent(in)    :: kgridCoarse
    type(greensfunc), intent(in)    :: Dzero
    type(greensfunc), intent(inout) :: Gzero
    integer, intent(out)            :: ierr
    !routine variables
    integer:: nx,ny,nz,nw2
    integer:: i,j,k,l
    real(real12) :: omega_diff

    nx=size(kgridCoarse,1)
    ny=size(kgridCoarse,2)
    nz=size(kgridCoarse,3)
    nw2=size(Dzero,4)
    
    allocate(Gzero(nx,ny,nz,nw2))
    !may have to rewrite for matrix form...
    forall (i=1:nx,j=1:ny,k=1:nz,l=1:nw2)
       startHybrid(i,j,k,l)=w2raw(l)-coarsew2(i,j,k)-1.0_real12/D0(i,j,k,l)
    end forall
   

  end subroutine initHybrid
    
end module initialisation
