! initialisation_fun.f90 - a unit test suite for initialisation.f90
!
! funit generated this file from initialisation.fun

module initialisation_fun
 
 use constants
 use definedtypes
 use initialisation

 implicit none

 logical :: noAssertFailed

 public :: test_initialisation

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



!global variables here
integer,  parameter          :: fine_points = 6   
integer,  parameter          :: ncoarse_cells = 2
integer,  parameter          :: nomega_points= 4
integer                      :: ierr
complex(real12), parameter   :: omax = (500.0_real12, zero) 
real(real12)                 :: tol = epsilon(one)
type(settingparam)           :: settings
type(storedparam)            :: stored
type(finegrid),allocatable   :: kgridFine(:,:,:)
type(coarsegrid),allocatable :: kgridCoarse(:,:,:)
type(greensfunc),allocatable :: Dzero(:,:,:,:)

 contains



 subroutine nf_points_lt_one_ierr

     settings%nfpoints = -10
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test nf_points_lt_one_ierr &
              &[initialisation.fun:36]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine nf_points_lt_one_ierr


 subroutine nomega_points_lt_one_ierr

     settings%nomega = -10
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test nomega_points_lt_one_ierr &
              &[initialisation.fun:42]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine nomega_points_lt_one_ierr


 subroutine real_omega_max_lt_real_omegamin_ierr

     settings%omegamax = cmplx(-one, zero)
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==2)) then
      print *, " *Assert_Equal failed* in test real_omega_max_lt_real_omegamin_ierr &
              &[initialisation.fun:48]"
      print *, "  ", "ierr (",ierr,") is not", 2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine real_omega_max_lt_real_omegamin_ierr


 subroutine kgridfine_not_allocated_ierr

     deallocate(kgridfine)
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==3)) then
      print *, " *Assert_Equal failed* in test kgridfine_not_allocated_ierr &
              &[initialisation.fun:54]"
      print *, "  ", "ierr (",ierr,") is not", 3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine kgridfine_not_allocated_ierr


 subroutine dzero_already_allocated_ierr

     allocate(Dzero(1,1,1,1))
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==4)) then
      print *, " *Assert_Equal failed* in test dzero_already_allocated_ierr &
              &[initialisation.fun:60]"
      print *, "  ", "ierr (",ierr,") is not", 4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine dzero_already_allocated_ierr


 subroutine allocate_ksize_correctly

     integer :: kfx, dzx
     integer :: kfy, dzy
     integer :: kfz, dzz
     logical :: kalloc_fail = .false.
     
     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     kfx = size(kgridFine, 1)
     kfy = size(kgridFine, 2)
     kfz = size(kgridFine, 3)

     dzx = size(Dzero, 1)
     dzy = size(Dzero, 2)
     dzz = size(Dzero, 3)

     if ((kfx.ne.dzx).or.(kfy.ne.dzy).or.(kfz.ne.dzz)) kalloc_fail = .true.
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (kalloc_fail) then
      print *, " *Assert_False failed* in test allocate_ksize_correctly &
              &[initialisation.fun:80]"
      print *, "  ", "kalloc_fail is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine allocate_ksize_correctly


 subroutine allocate_nomega_correctly

     integer :: nomsz

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     nomsz = size(Dzero, 4)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(nomsz== nomega_points)) then
      print *, " *Assert_Equal failed* in test allocate_nomega_correctly &
              &[initialisation.fun:89]"
      print *, "  ", "nomsz (",nomsz,") is not",  nomega_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine allocate_nomega_correctly


 subroutine correct_omega_diff

     complex(real12) :: omegadiff_test
     complex(real12) :: omegamax_test

     settings%omegaMax = cmplx(one, one)
     omegadiff_test = settings%omegaMax - settings%omegaMin
     omegadiff_test = omegadiff_test/real(nomega_points)

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(stored%omega_diff) &
     +tol) &
     .ge. &
     (real(omegadiff_test)) &
             .and. &
     (real(stored%omega_diff) &
     -tol) &
     .le. &
     (real(omegadiff_test)) )) then
      print *, " *Assert_Equal_Within failed* in test correct_omega_diff &
              &[initialisation.fun:102]"
      print *, "  ", "real(omegadiff_test) (",real(omegadiff_test),") is not", &
 real(stored%omega_diff),"within",tol
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(stored%omega_diff) &
     +tol) &
     .ge. &
     (aimag(omegadiff_test)) &
             .and. &
     (aimag(stored%omega_diff) &
     -tol) &
     .le. &
     (aimag(omegadiff_test)) )) then
      print *, " *Assert_Equal_Within failed* in test correct_omega_diff &
              &[initialisation.fun:103]"
      print *, "  ", "aimag(omegadiff_test) (",aimag(omegadiff_test),") is not", &
 aimag(stored%omega_diff),"within",tol
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     omegamax_test=omegadiff_test*real(nomega_points)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(settings%omegaMax) &
     +tol) &
     .ge. &
     (real(omegamax_test)) &
             .and. &
     (real(settings%omegaMax) &
     -tol) &
     .le. &
     (real(omegamax_test)) )) then
      print *, " *Assert_Equal_Within failed* in test correct_omega_diff &
              &[initialisation.fun:106]"
      print *, "  ", "real(omegamax_test) (",real(omegamax_test),") is not", &
 real(settings%omegaMax),"within",tol
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(settings%omegaMax) &
     +tol) &
     .ge. &
     (aimag(omegamax_test)) &
             .and. &
     (aimag(settings%omegaMax) &
     -tol) &
     .le. &
     (aimag(omegamax_test)) )) then
      print *, " *Assert_Equal_Within failed* in test correct_omega_diff &
              &[initialisation.fun:107]"
      print *, "  ", "aimag(omegamax_test) (",aimag(omegamax_test),") is not", &
 aimag(settings%omegaMax),"within",tol
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine correct_omega_diff


 subroutine mappings_copied_correctly

     integer :: kgridmap
     integer :: Dzmap
     integer :: ix, iy, iz, inom
     logical :: dzmap_fail = .false.

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     do ix = 1, fine_points
     	if (dzmap_fail) exit
     	do iy = 1, fine_points
	   if (dzmap_fail) exit
	   do iz = 1, fine_points
	      if (dzmap_fail) exit
	      do inom = 1, nomega_points
	      	 kgridmap=kgridFine(ix,iy,iz)%coarseMap
		 Dzmap=Dzero(ix,iy,iz,inom)%map
	      	 if (kgridmap.ne.Dzmap) then
		    dzmap_fail = .false.
		    exit
		 endif
	      enddo
	   enddo
	enddo
     enddo

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (dzmap_fail) then
      print *, " *Assert_False failed* in test mappings_copied_correctly &
              &[initialisation.fun:136]"
      print *, "  ", "dzmap_fail is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine mappings_copied_correctly


 subroutine funit_setup
    !code that should be run before each test
    settings%nfpoints = fine_points
    settings%ncell = ncoarse_cells
    settings%nomega = nomega_points
    settings%omegaMax = omax
    call initgrid(settings,kgridFine,kgridCoarse,ierr)
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
    !code that runs after each test

    if (allocated(kgridFine)) deallocate(kgridFine)
    if (allocated(kgridCoarse)) deallocate(kgridCoarse)
    if (allocated(Dzero)) deallocate(Dzero)
 end subroutine funit_teardown


 subroutine test_initialisation( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call nf_points_lt_one_ierr
  call funit_teardown

  call funit_setup
  call nomega_points_lt_one_ierr
  call funit_teardown

  call funit_setup
  call real_omega_max_lt_real_omegamin_ierr
  call funit_teardown

  call funit_setup
  call kgridfine_not_allocated_ierr
  call funit_teardown

  call funit_setup
  call dzero_already_allocated_ierr
  call funit_teardown

  call funit_setup
  call allocate_ksize_correctly
  call funit_teardown

  call funit_setup
  call allocate_nomega_correctly
  call funit_teardown

  call funit_setup
  call correct_omega_diff
  call funit_teardown

  call funit_setup
  call mappings_copied_correctly
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_initialisation

end module initialisation_fun
