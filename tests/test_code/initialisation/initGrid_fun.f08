! initialisation_fun.f90 - a unit test suite for initialisation.f90
!
! funit generated this file from initialisation.fun

module initialisation_fun

 use initialisation
 use definedtypes

 implicit none

 logical :: noAssertFailed

 public :: test_initialisation

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



!global variables here
!use derivedtypes
integer,  parameter :: fine_points=5
integer,  parameter :: ncoarse_cells=5
type(settingparam)  :: settings
type(finegrid),allocatable     :: kgridFine(:,:,:)
type(coarsegrid),allocatable   :: kgridCoarse(:,:,:)


 contains



 subroutine fine_points_stored_correctly

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%nfpoints(1)==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_points_stored_correctly &
              &[initialisation.fun:32]"
      print *, "  ", "settings%nfpoints(1) (",settings%nfpoints(1),") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%nfpoints(2)==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_points_stored_correctly &
              &[initialisation.fun:33]"
      print *, "  ", "settings%nfpoints(2) (",settings%nfpoints(2),") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%nfpoints(3)==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_points_stored_correctly &
              &[initialisation.fun:34]"
      print *, "  ", "settings%nfpoints(3) (",settings%nfpoints(3),") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_points_stored_correctly


 subroutine coarse_cell_no_stored_correctly

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%ncell(1)==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_cell_no_stored_correctly &
              &[initialisation.fun:38]"
      print *, "  ", "settings%ncell(1) (",settings%ncell(1),") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%ncell(2)==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_cell_no_stored_correctly &
              &[initialisation.fun:39]"
      print *, "  ", "settings%ncell(2) (",settings%ncell(2),") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%ncell(3)==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_cell_no_stored_correctly &
              &[initialisation.fun:40]"
      print *, "  ", "settings%ncell(3) (",settings%ncell(3),") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine coarse_cell_no_stored_correctly


 subroutine fine_grid_array_size

  integer:: ix,iy,iz

  ix=size(kgridFine,1)
  iy=size(kgridFine,2)
  iz=size(kgridFine,3)

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ix==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_grid_array_size &
              &[initialisation.fun:50]"
      print *, "  ", "ix (",ix,") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(iy==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_grid_array_size &
              &[initialisation.fun:51]"
      print *, "  ", "iy (",iy,") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(iz==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_grid_array_size &
              &[initialisation.fun:52]"
      print *, "  ", "iz (",iz,") is not", fine_points
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine fine_grid_array_size


 subroutine coarse_grid_array_size

  integer:: ix,iy,iz

  ix=size(kgridCoarse,1)
  iy=size(kgridCoarse,2)
  iz=size(kgridCoarse,3)

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ix==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_grid_array_size &
              &[initialisation.fun:63]"
      print *, "  ", "ix (",ix,") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(iy==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_grid_array_size &
              &[initialisation.fun:64]"
      print *, "  ", "iy (",iy,") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(iz==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_grid_array_size &
              &[initialisation.fun:65]"
      print *, "  ", "iz (",iz,") is not", ncoarse_cells
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine coarse_grid_array_size


 subroutine funit_setup
  use definedtypes               
  !code that should be run before each test
  settings%nfpoints=fine_points
  settings%ncell=ncoarse_cells
  call initgrid(settings,kgridFine,kgridCoarse)
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
  !code runs after each test
  integer::stat

  deallocate(kgridFine,kgridCoarse)
  open(unit=1234, iostat=stat, file="finekgrid.out", status='unknown')
  close(1234, status='delete')
  open(unit=1234, iostat=stat, file="coarsekgrid.out", status='unknown')
  close(1234, status='delete')
 end subroutine funit_teardown


 subroutine test_initialisation( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call fine_points_stored_correctly
  call funit_teardown

  call funit_setup
  call coarse_cell_no_stored_correctly
  call funit_teardown

  call funit_setup
  call fine_grid_array_size
  call funit_teardown

  call funit_setup
  call coarse_grid_array_size
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_initialisation

end module initialisation_fun
