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



!global variables here (don't change fine_points or ncoarse_cells)
integer,  parameter :: fine_points=6   
integer,  parameter :: ncoarse_cells=2 
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
              &[initialisation.fun:26]"
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
              &[initialisation.fun:27]"
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
              &[initialisation.fun:28]"
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
              &[initialisation.fun:32]"
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
              &[initialisation.fun:33]"
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
              &[initialisation.fun:34]"
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
              &[initialisation.fun:44]"
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
              &[initialisation.fun:45]"
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
              &[initialisation.fun:46]"
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
              &[initialisation.fun:57]"
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
              &[initialisation.fun:58]"
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
              &[initialisation.fun:59]"
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


 subroutine fine_grid_values_corners

  real(real12),parameter::first=-pi/3.0_real12
  real(real12),parameter::last=4.0_real12*pi/3.0_real12
  real(real12),parameter::tolerance=epsilon(pi)
  real(real12)::testx,testy,testz

  testx=kgridFine(1,1,1)%kx
  testy=kgridFine(1,1,1)%ky
  testz=kgridFine(1,1,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:72]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:73]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:74]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(fine_points,1,1)%kx
  testy=kgridFine(fine_points,1,1)%ky
  testz=kgridFine(fine_points,1,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:79]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:80]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:81]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(1,fine_points,1)%kx
  testy=kgridFine(1,fine_points,1)%ky
  testz=kgridFine(1,fine_points,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:86]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:87]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:88]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(1,1,fine_points)%kx
  testy=kgridFine(1,1,fine_points)%ky
  testz=kgridFine(1,1,fine_points)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:93]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:94]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:95]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(fine_points,fine_points,1)%kx
  testy=kgridFine(fine_points,fine_points,1)%ky
  testz=kgridFine(fine_points,fine_points,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:100]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:101]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:102]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(fine_points,1,fine_points)%kx
  testy=kgridFine(fine_points,1,fine_points)%ky
  testz=kgridFine(fine_points,1,fine_points)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:107]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:108]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:109]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(1,fine_points,fine_points)%kx
  testy=kgridFine(1,fine_points,fine_points)%ky
  testz=kgridFine(1,fine_points,fine_points)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:114]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:115]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:116]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridFine(fine_points,fine_points,fine_points)%kx
  testy=kgridFine(fine_points,fine_points,fine_points)%ky
  testz=kgridFine(fine_points,fine_points,fine_points)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:121]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:122]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test fine_grid_values_corners &
              &[initialisation.fun:123]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  

  numTests = numTests + 1

 end subroutine fine_grid_values_corners

  


 subroutine coarse_grid_values_corners

  real(real12),parameter::first=0.0_real12
  real(real12),parameter::last=pi
  real(real12),parameter::tolerance=epsilon(pi)
  real(real12)::testx,testy,testz

  testx=kgridCoarse(1,1,1)%kx
  testy=kgridCoarse(1,1,1)%ky
  testz=kgridCoarse(1,1,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:139]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:140]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:141]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(ncoarse_cells,1,1)%kx
  testy=kgridCoarse(ncoarse_cells,1,1)%ky
  testz=kgridCoarse(ncoarse_cells,1,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:146]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:147]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:148]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(1,ncoarse_cells,1)%kx
  testy=kgridCoarse(1,ncoarse_cells,1)%ky
  testz=kgridCoarse(1,ncoarse_cells,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:153]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:154]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:155]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(1,1,ncoarse_cells)%kx
  testy=kgridCoarse(1,1,ncoarse_cells)%ky
  testz=kgridCoarse(1,1,ncoarse_cells)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:160]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:161]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:162]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kx
  testy=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%ky
  testz=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:167]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:168]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:169]"
      print *, "  ", "testz (",testz,") is not", &
 first,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kx
  testy=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%ky
  testz=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:174]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:175]"
      print *, "  ", "testy (",testy,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:176]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kx
  testy=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%ky
  testz=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((first &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (first &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:181]"
      print *, "  ", "testx (",testx,") is not", &
 first,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:182]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:183]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  testx=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kx
  testy=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%ky
  testz=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kz
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testx) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testx) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:188]"
      print *, "  ", "testx (",testx,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testy) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testy) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:189]"
      print *, "  ", "testy (",testy,") is not", &
 last,"within",tolerance
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
    if (.not.((last &
     +tolerance) &
     .ge. &
     (testz) &
             .and. &
     (last &
     -tolerance) &
     .le. &
     (testz) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_grid_values_corners &
              &[initialisation.fun:190]"
      print *, "  ", "testz (",testz,") is not", &
 last,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine coarse_grid_values_corners


 subroutine assigncell_maps

   integer,parameter::nfine=27
   integer,parameter::maxcells=ncoarse_cells**3
   integer,allocatable::cellCount(:,:,:)
   integer:: icell,testno
   logical:: npwrong

   !test the number of points mapped to each cell is correct
   allocate(cellcount(fine_points,fine_points,fine_points))
   !loop over cells
   npwrong=.false.
   do icell=1,maxcells
      cellcount=0
      where (kgridFine%coarsemap==icell) cellcount=1
      testno=sum(cellcount)
      if (testno.ne.nfine) then
      	 npwrong=.true.
	 exit
      endif
   enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (npwrong) then
      print *, " *Assert_False failed* in test assigncell_maps &
              &[initialisation.fun:214]"
      print *, "  ", "npwrong is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   deallocate(cellcount)

  numTests = numTests + 1

 end subroutine assigncell_maps


 subroutine assigncell_specific_fine_points

   integer::testcell
   
   !test specific cells
   testcell=kgridfine(5,5,4)%coarsemap
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==8)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:223]"
      print *, "  ", "testcell (",testcell,") is not", 8
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,1,1)%coarsemap
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==1)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:225]"
      print *, "  ", "testcell (",testcell,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,1,6)%coarsemap
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==5)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:227]"
      print *, "  ", "testcell (",testcell,") is not", 5
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,6,6)%coarsemap
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==7)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:229]"
      print *, "  ", "testcell (",testcell,") is not", 7
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(6,6,6)%coarsemap
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==8)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:231]"
      print *, "  ", "testcell (",testcell,") is not", 8
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine assigncell_specific_fine_points

    
 subroutine assign_coarse_labels

   integer::ix,iy,iz,itest
   logical::problem

   problem=.false.
   do ix=1,ncoarse_cells
      if (problem) exit
      do iy=1,ncoarse_cells
        if (problem) exit
        do iz=1,ncoarse_cells
           itest=ix+(iy-1)*ncoarse_cells+(iz-1)*ncoarse_cells*ncoarse_cells
	   if (itest.ne.kgridcoarse(ix,iy,iz)%label) then
	      problem=.true.
	      exit
	   endif
	enddo
      enddo
   enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test assign_coarse_labels &
              &[initialisation.fun:253]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif



  numTests = numTests + 1

 end subroutine assign_coarse_labels


 subroutine funit_setup
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

  call funit_setup
  call fine_grid_values_corners
  call funit_teardown

  call funit_setup
  call coarse_grid_values_corners
  call funit_teardown

  call funit_setup
  call assigncell_maps
  call funit_teardown

  call funit_setup
  call assigncell_specific_fine_points
  call funit_teardown

  call funit_setup
  call assign_coarse_labels
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_initialisation

end module initialisation_fun
