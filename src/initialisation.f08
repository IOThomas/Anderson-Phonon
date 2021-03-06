module initialisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Contains initialisation subroutines for the calculation.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   use constants, only: real12, pi, half, two, zero, one, cmplx_zero, fatal_error_from_call
   use dispersions, only: finedispersion, coarseDispersion
   use definedtypes, only: settingparam, kappagrid, storedparam
   use greensroutines, only: allocate_3DGF,  calculateGF, greensfunc, reduceGF, invertGF
   implicit none
   private
   public initGrid, initDzero, initHybrid

contains
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine initGrid(settings, kgridFine, kgridCoarse, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Initialises coarse and fine momentum grids for the calculation.  
!# Note that the formula for assigning the kpoint values is a little non-obvious
!# and only works for *even* values of Ncell and Nfpoints.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr=0: Routine executed fine;
!#              ierr=1: Ncell or Nfpoints are negative or zero;
!#              ierr=2: Ncell or Nfpoints are odd;
!#              ierr=3: kgridFine or kgridCoarse already allocated.    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      type(settingparam), intent(in)               :: settings
      !# contains required settings information
      type(kappagrid), allocatable, intent(inout)  :: kgridFine(:,:,:)
      !# fine momentum space grid
      type(kappagrid), allocatable, intent(inout)  :: kgridCoarse(:,:,:)
      !# coarse momentum space grid
      integer                                      :: ierr
      !# error code
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
      elseif (allocated(kgridFine).or.allocated(kgridCoarse)) then
         ierr=3
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
               kgridCoarse(ix, iy, iz)%map = ix + (iy -1 ) * itemp(1)&
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
               where ((kgridFine%kx >= lowLim(1)).and.(kgridFine%kx < upperLim(1))&
                  .and.(kgridFine%ky >= lowLim(2)).and.(kgridFine%ky < upperLim(2))&
                  .and.(kgridFine%kz >= lowLim(3)).and.(kgridFine%kz < upperLim(3)))   

                  kgridFine%map = kgridCoarse(ix, iy, iz)%map
                  tempArray = kgridFine%omega2
               end where
               kgridCoarse(ix, iy, iz)%omega2 = coarseDispersion(tempArray, iNcell, iNpoint)
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
                    kgridFine(ix, iy, iz)%omega2, kgridFine(ix, iy, iz)%map
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
                    kgridCoarse(ix, iy, iz)%omega2,kgridCoarse(ix, iy, iz)%map
            enddo
         enddo
      enddo
      close(10)      
      deallocate(tempArray)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!!!!!!!!!!!!!!!!!
! FORMAT BLOCKS !
!!!!!!!!!!!!!!!!!
800   format(3i5, 4es25.15, 2es25.15, i5)
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
      squaregridmom = (real(ipoint, real12) - half * real( 1 + (itotal/ 2), real12)) * tlength
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      end function squaregridmom
!------------------------------------------------------------------------------ 
   end subroutine initGrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine initDzero(settings, kgridFine, stored, Dzero, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Initialises fine grid non-interacting Green's functions.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- no problems;
!#              ierr = 1 -- settings%nfpoints or settings%nomega less than one; 
!#              ierr = 2 -- settings%omegamax less than settings%omegamin;
!#              ierr = 3 -- kgridFine not allocated;
!#              ierr = 4 -- Dzero already allocated;
!#              ierr = 5 -- Division by zero when calculating Dzero%GF (program calls fatal error)    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      type(settingparam), intent(inout)           :: settings
      !# contains required settings information
      type(kappagrid), allocatable, intent(in)    :: kgridFine(:,:,:)
      !# fine momentum grid
      type(storedparam), intent(inout)            :: stored
      !# stored parameters
      type(greensfunc),allocatable, intent(inout) :: Dzero(:,:,:)
      !# non-interacting Green's Function
      integer                                     :: ierr
      !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer         :: nfpoints(3)
      integer         :: nomega
      integer         :: icond(3)
      integer         :: ier1
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
      elseif (real(settings%omegaMax).le.real(settings%omegaMin)) then
         ierr=2
         return
      elseif (.not.allocated(kgridFine)) then
         ierr=3
         return
      elseif (allocated(Dzero)) then
         ierr=4
         return
      else
         ierr=0
      end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! allocate and calculate values of Dzero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      omega_diff = (settings%omegaMax - settings%omegaMin) / real(nomega, real12)
      stored%omega_diff=omega_diff
    
      call allocate_3DGF(Dzero, nfpoints(1), nfpoints(2), nfpoints(3), nomega, ier1)
      if (ier1.ne.0) call fatal_error_from_call(ier1, 'initDzero', 'allocate_3DGF')

      call calculateGF(GFval = Dzero, deltaw = omega_diff, dispersion = kgridFine, ierr = ier1)
      if (ier1.ne.0) call fatal_error_from_call(ier1, 'initDzero', 'calculateGF')

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   end subroutine initDzero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine initHybrid(stored, kgridCoarse, Dzero, Gzero, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Initialises the coarse grid hybridisation function.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- no problems; 
!#              ierr = 1 -- stored%omega_diff less than or equal to zero;    
!#              ierr = 2 -- either or both of kgridCoarse and Dzero unallocated;
!#              ierr = 3 -- Gzero already allocated;
!#              ierr = 4 -- fine gridpoints in at least one direction <= # of
!#                          coarse gridpoints.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      type(storedparam), intent(in)                :: stored
      !# contains required settings information
      type(kappagrid), allocatable, intent(in)     :: kgridCoarse(:, :, :)
      !# coarse momentum grid
      type(greensfunc), allocatable, intent(in)    :: Dzero(:, :, :)
      !# fine grid none-interacting Green's function
      type(greensfunc), allocatable, intent(inout) :: Gzero(:, :, :)
      !# coarse grid hybridisation function
      integer, intent(out)                         :: ierr
      !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer                       :: nx, ny, nz, nw2
      integer                       :: nxfin, nyfin, nzfin
      integer                       :: i, j, k, l
      integer                       :: ier1
      complex(real12)               :: omega_diff
      integer, allocatable          :: ier2(:, :, :)
      type(greensfunc), allocatable :: work(:, :, :)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      omega_diff = stored%omega_diff
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! input error tests
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      if (real(omega_diff).le.zero) then
         ierr = 1
         return
      elseif ((.not.allocated(kgridCoarse)).or.(.not.allocated(Dzero))) then
         ierr = 2
         return
      elseif (allocated(Gzero)) then
         ierr = 3
         return
      else
         ierr = 0
      end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! array allocation and coarse point label assignment
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      nx = size(kgridCoarse, 1)
      ny = size(kgridCoarse, 2)
      nz = size(kgridCoarse, 3)
      allocate(ier2(nx, ny, nz))
    
      nxfin = size(Dzero, 1)
      nyfin = size(Dzero, 2)
      nzfin = size(Dzero, 3)
      nw2 = size(Dzero(1, 1, 1)%GF, 1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      check_grid_sizes:if ((nx.ge.nxfin).or.(ny.ge.nyfin).or.(nz.ge.nzfin)) then
         ierr = 4
         return
      end if check_grid_sizes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      call allocate_3DGF(Gzero, nx, ny, nz, nw2, ier1)
      if (ier1.ne.0) call fatal_error_from_call(ier1, 'initHybrid', 'allocate_3DGF')
      call allocate_3DGF(work, nx, ny, nz, nw2, ier1)
      if (ier1.ne.0) call fatal_error_from_call(ier1, 'initHybrid', 'allocate_3DGF')

      Gzero(1:nx, 1:ny, 1:nz)%map=kgridCoarse(1:nx, 1:ny, 1:nz)%map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! calculate Gzero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      ier1=0 !set error flags to zero
      ier2=0 

      call reduceGF(work, Dzero, ier1)
      if (ier1.ne.0) call fatal_error_from_call(ier1, 'initHybrid', 'reduceGF')
        
      call invertGF(work, ier2)
      ! only one possible error code from this
      if (any(ier2.eq.1)) call fatal_error_from_call(1, 'initHybrid', 'invertGF')

      !may have to rewrite for matrix form...
      forall (i=1:nx, j=1:ny, k=1:nz, l=1:nw2)
         Gzero(i,j,k)%GF(l) = real(l**2,real12)*omega_diff**2 - kgridCoarse(i, j, k)%omega2 - work(i,j,k)%GF(l)
      end forall

      deallocate(work, ier2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   end subroutine initHybrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module initialisation
