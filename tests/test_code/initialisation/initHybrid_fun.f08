! initialisation_fun.f90 - a unit test suite for initialisation.f90
!
! funit generated this file from initialisation.fun

module initialisation_fun

 use constants
 use definedtypes
 use greensroutines
 use initialisation

 implicit none

 logical :: noAssertFailed

 public :: test_initialisation

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



!global variables
integer,  parameter           :: fine_points = 4   
integer,  parameter           :: ncoarse_cells = 2
integer,  parameter           :: nomega_points= 4
integer                       :: ierr
complex(real12), parameter    :: omax = cmplx(500.0_real12, zero) 
type(settingparam)            :: settings
type(storedparam)             :: stored
type(kappagrid), allocatable  :: kgridFine(:, :, :)
type(kappagrid), allocatable  :: kgridCoarse(:, :, :)
type(greensfunc), allocatable :: Dzero(:, :, :)
type(greensfunc), allocatable :: Hybrid(:, :, :)

 contains



 subroutine storedomega_ierr_1


! these should give ierr = 1
     stored%omega_diff = cmplx(zero, zero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test storedomega_ierr_1 &
              &[initialisation.fun:57]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     stored%omega_diff = cmplx(zero, one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test storedomega_ierr_1 &
              &[initialisation.fun:61]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

     stored%omega_diff = cmplx(zero, -one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test storedomega_ierr_1 &
              &[initialisation.fun:66]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     stored%omega_diff = cmplx(-one, zero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test storedomega_ierr_1 &
              &[initialisation.fun:70]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     stored%omega_diff = cmplx(-one, -one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test storedomega_ierr_1 &
              &[initialisation.fun:74]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     


  numTests = numTests + 1

 end subroutine storedomega_ierr_1


 subroutine kgrid_deallocated_ierr2


     deallocate(kgridCoarse)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test kgrid_deallocated_ierr2 &
              &[initialisation.fun:83]"
      print *, "  ", "ierr (",ierr,") is not",  2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine kgrid_deallocated_ierr2


 subroutine Dzero_deallocated_ierr2


     deallocate(Dzero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test Dzero_deallocated_ierr2 &
              &[initialisation.fun:91]"
      print *, "  ", "ierr (",ierr,") is not",  2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine Dzero_deallocated_ierr2


 subroutine Dzero_kgrid_deallocated_ierr2


     deallocate(Dzero)
     deallocate(kgridCoarse)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test Dzero_kgrid_deallocated_ierr2 &
              &[initialisation.fun:100]"
      print *, "  ", "ierr (",ierr,") is not",  2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine Dzero_kgrid_deallocated_ierr2


 subroutine Gzero_allocated_ierr3


     call allocateGF(Hybrid, ncoarse_cells, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 3)) then
      print *, " *Assert_Equal failed* in test Gzero_allocated_ierr3 &
              &[initialisation.fun:109]"
      print *, "  ", "ierr (",ierr,") is not",  3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine Gzero_allocated_ierr3


 subroutine finepoints_le_coarsepoints_ierr4


     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, fine_points, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:120]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, fine_points, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:126]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, ncoarse_cells, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:133]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, ncoarse_cells-1, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:139]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, fine_points, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:146]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, fine_points, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:152]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, ncoarse_cells, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:159]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, ncoarse_cells-1, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:165]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, fine_points, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:172]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, fine_points, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:178]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:185]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, ncoarse_cells-1, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:191]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:198]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, ncoarse_cells-1, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 4)) then
      print *, " *Assert_Equal failed* in test finepoints_le_coarsepoints_ierr4 &
              &[initialisation.fun:204]"
      print *, "  ", "ierr (",ierr,") is not",  4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine finepoints_le_coarsepoints_ierr4


 subroutine output_eqn

     integer      :: i, j, k, l
     integer      :: ic, jc, kc
     real(real12) :: real_output(4) = (/ -1.0625_real12, 1.9375_real12, &
     		  6.9375_real12, 13.9375_real12 /)
     real(real12) :: imag_output = -1.9375_real12
     logical      :: output_problem = .false.
     logical      :: mapping_problem = .false.


! initialisation
     do i = 1, ncoarse_cells
     	do j = 1, ncoarse_cells
	   do k = 1, ncoarse_cells
	      do l = 1, nomega_points
	      	 kgridCoarse(i, j, k)%omega2 = cmplx(2.0_real12, 2.0_real12)
	      enddo
	   enddo
	enddo
     enddo

     do i = 1, fine_points
        do j = 1, fine_points
           do k = 1, fine_points
	      ic = nint(real(i)/two)
              jc = nint(real(j)/two)
              kc = nint(real(k)/two)
              Dzero(i , j , k)%map = ic + (jc - 1)*ncoarse_cells &
                  + (kc - 1)*ncoarse_cells*ncoarse_cells
              do l = 1, nomega_points
                 Dzero(i, j, k)%GF(l) = cmplx(one, one)
              enddo
           enddo
        enddo
     enddo

     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)

! checks
     do i = 1, ncoarse_cells
     	if (output_problem) exit
     	do j = 1, ncoarse_cells
	   if (output_problem) exit
	   do k = 1, ncoarse_cells
	      if (output_problem) exit
	      do l = 1, nomega_points
                 if (real(Hybrid(i, j, k)%GF(l)).ne.real_output(l)) then
		    output_problem = .true.
		    exit
		 endif
		 if (aimag(Hybrid(i, j, k)%GF(l)).ne.imag_output) then
		    output_problem = .true.
		    exit
		 endif
              enddo
	   enddo
	enddo
     enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (output_problem) then
      print *, " *Assert_False failed* in test output_eqn &
              &[initialisation.fun:265]"
      print *, "  ", "output_problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
 
     do i = 1, ncoarse_cells
     	if (mapping_problem) exit
     	do j = 1, ncoarse_cells
	   if (mapping_problem) exit
	   do k = 1, ncoarse_cells
	      if (Hybrid(i, j, k)%map.ne.kgridCoarse(i, j, k)%map) then
	      	 mapping_problem = .true.
		 exit
	      endif
	   enddo
	enddo
     enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (mapping_problem) then
      print *, " *Assert_False failed* in test output_eqn &
              &[initialisation.fun:279]"
      print *, "  ", "mapping_problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine output_eqn

     

 subroutine funit_setup
    integer :: i, j, k
    !code that should be run before each test
    settings%nfpoints = fine_points
    settings%ncell = ncoarse_cells
    settings%nomega = nomega_points
    settings%omegaMax = omax
    stored%omega_diff = cmplx(one, zero)
    allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
    do i = 1, ncoarse_cells
        do j = 1, ncoarse_cells
	     do k = 1, ncoarse_cells
	             kgridCoarse(i, j, k)%kx = one
	             kgridCoarse(i, j, k)%ky = one
	             kgridCoarse(i, j, k)%kz = one
	             kgridCoarse(i, j, k)%norm = one
	             kgridCoarse(i, j, k)%map = i + (j - 1)*ncoarse_cells&
	                  + (k - 1)*ncoarse_cells*ncoarse_cells
             enddo
        enddo
    enddo

    call allocateGF(Dzero, fine_points, fine_points, fine_points, &
        nomega_points, ierr)
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
    !code that runs after each test

    if (allocated(kgridFine)) deallocate(kgridFine)
    if (allocated(kgridCoarse)) deallocate(kgridCoarse)
    if (allocated(Dzero)) deallocate(Dzero)
    if (allocated(Hybrid)) deallocate(Hybrid)
   
 end subroutine funit_teardown


 subroutine test_initialisation( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call storedomega_ierr_1
  call funit_teardown

  call funit_setup
  call kgrid_deallocated_ierr2
  call funit_teardown

  call funit_setup
  call Dzero_deallocated_ierr2
  call funit_teardown

  call funit_setup
  call Dzero_kgrid_deallocated_ierr2
  call funit_teardown

  call funit_setup
  call Gzero_allocated_ierr3
  call funit_teardown

  call funit_setup
  call finepoints_le_coarsepoints_ierr4
  call funit_teardown

  call funit_setup
  call output_eqn
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_initialisation

end module initialisation_fun
