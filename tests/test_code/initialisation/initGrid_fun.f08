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
integer             :: ierr
type(settingparam)  :: settings
type(kappagrid),allocatable     :: kgridFine(:,:,:)
type(kappagrid),allocatable   :: kgridCoarse(:,:,:)


 contains



 subroutine negative_finegrid_ierr

  settings%nfpoints(3)=-1
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test negative_finegrid_ierr &
              &[initialisation.fun:29]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine negative_finegrid_ierr


 subroutine odd_finegrid_ierr

  settings%nfpoints(3)=5
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==2)) then
      print *, " *Assert_Equal failed* in test odd_finegrid_ierr &
              &[initialisation.fun:35]"
      print *, "  ", "ierr (",ierr,") is not", 2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine odd_finegrid_ierr


 subroutine not_allocatated_fine_grid_ierr

     allocate(kgridFine(fine_points,fine_points,fine_points))
     call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==3)) then
      print *, " *Assert_Equal failed* in test not_allocatated_fine_grid_ierr &
              &[initialisation.fun:41]"
      print *, "  ", "ierr (",ierr,") is not", 3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine not_allocatated_fine_grid_ierr


 subroutine not_allocatated_coarse_grid_ierr

     allocate(kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells))
     call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==3)) then
      print *, " *Assert_Equal failed* in test not_allocatated_coarse_grid_ierr &
              &[initialisation.fun:47]"
      print *, "  ", "ierr (",ierr,") is not", 3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine not_allocatated_coarse_grid_ierr


 subroutine negative_coarsegrid_ierr

  settings%ncell(3)=-1
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test negative_coarsegrid_ierr &
              &[initialisation.fun:53]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine negative_coarsegrid_ierr



 subroutine odd_coarsegrid_ierr

  settings%ncell(3)=5
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==2)) then
      print *, " *Assert_Equal failed* in test odd_coarsegrid_ierr &
              &[initialisation.fun:60]"
      print *, "  ", "ierr (",ierr,") is not", 2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine odd_coarsegrid_ierr


 subroutine fine_points_stored_correctly

  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%nfpoints(1)==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_points_stored_correctly &
              &[initialisation.fun:65]"
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
              &[initialisation.fun:66]"
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
              &[initialisation.fun:67]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(settings%ncell(1)==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_cell_no_stored_correctly &
              &[initialisation.fun:73]"
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
              &[initialisation.fun:74]"
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
              &[initialisation.fun:75]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  ix=size(kgridFine,1)
  iy=size(kgridFine,2)
  iz=size(kgridFine,3)

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ix==fine_points)) then
      print *, " *Assert_Equal failed* in test fine_grid_array_size &
              &[initialisation.fun:88]"
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
              &[initialisation.fun:89]"
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
              &[initialisation.fun:90]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  ix=size(kgridCoarse,1)
  iy=size(kgridCoarse,2)
  iz=size(kgridCoarse,3)

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ix==ncoarse_cells)) then
      print *, " *Assert_Equal failed* in test coarse_grid_array_size &
              &[initialisation.fun:103]"
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
              &[initialisation.fun:104]"
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
              &[initialisation.fun:105]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

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
              &[initialisation.fun:120]"
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
              &[initialisation.fun:121]"
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
              &[initialisation.fun:122]"
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
              &[initialisation.fun:127]"
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
              &[initialisation.fun:128]"
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
              &[initialisation.fun:129]"
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
              &[initialisation.fun:134]"
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
              &[initialisation.fun:135]"
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
              &[initialisation.fun:136]"
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
              &[initialisation.fun:141]"
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
              &[initialisation.fun:142]"
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
              &[initialisation.fun:143]"
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
              &[initialisation.fun:148]"
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
              &[initialisation.fun:149]"
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
              &[initialisation.fun:150]"
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
              &[initialisation.fun:155]"
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
              &[initialisation.fun:156]"
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
              &[initialisation.fun:157]"
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
              &[initialisation.fun:162]"
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
              &[initialisation.fun:163]"
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
              &[initialisation.fun:164]"
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
              &[initialisation.fun:169]"
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
              &[initialisation.fun:170]"
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
              &[initialisation.fun:171]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

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
              &[initialisation.fun:189]"
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
              &[initialisation.fun:190]"
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
              &[initialisation.fun:191]"
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
              &[initialisation.fun:196]"
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
              &[initialisation.fun:197]"
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
              &[initialisation.fun:198]"
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
              &[initialisation.fun:203]"
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
              &[initialisation.fun:204]"
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
              &[initialisation.fun:205]"
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
              &[initialisation.fun:210]"
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
              &[initialisation.fun:211]"
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
              &[initialisation.fun:212]"
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
              &[initialisation.fun:217]"
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
              &[initialisation.fun:218]"
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
              &[initialisation.fun:219]"
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
              &[initialisation.fun:224]"
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
              &[initialisation.fun:225]"
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
              &[initialisation.fun:226]"
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
              &[initialisation.fun:231]"
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
              &[initialisation.fun:232]"
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
              &[initialisation.fun:233]"
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
              &[initialisation.fun:238]"
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
              &[initialisation.fun:239]"
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
              &[initialisation.fun:240]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

   !test the number of points mapped to each cell is correct
   allocate(cellcount(fine_points,fine_points,fine_points))
   !loop over cells
   npwrong=.false.
   do icell=1,maxcells
      cellcount=0
      where (kgridFine%map==icell) cellcount=1
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
              &[initialisation.fun:266]"
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

  call initgrid(settings,kgridFine,kgridCoarse,ierr)
   
   !test specific cells
   testcell=kgridfine(5,5,4)%map
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==8)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:278]"
      print *, "  ", "testcell (",testcell,") is not", 8
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,1,1)%map
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==1)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:280]"
      print *, "  ", "testcell (",testcell,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,1,6)%map
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==5)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:282]"
      print *, "  ", "testcell (",testcell,") is not", 5
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(1,6,6)%map
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==7)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:284]"
      print *, "  ", "testcell (",testcell,") is not", 7
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
   testcell=kgridfine(6,6,6)%map
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(testcell==8)) then
      print *, " *Assert_Equal failed* in test assigncell_specific_fine_points &
              &[initialisation.fun:286]"
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

   call initgrid(settings,kgridFine,kgridCoarse,ierr)

   problem=.false.
   do ix=1,ncoarse_cells
      if (problem) exit
      do iy=1,ncoarse_cells
        if (problem) exit
        do iz=1,ncoarse_cells
           itest=ix+(iy-1)*ncoarse_cells+(iz-1)*ncoarse_cells*ncoarse_cells
	   if (itest.ne.kgridcoarse(ix,iy,iz)%map) then
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
              &[initialisation.fun:310]"
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


 subroutine fine_points_reals_are_numbers

  logical::problem
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kx)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:332]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%ky)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:345]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:358]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
	   
  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:371]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%norm)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:384]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(real(kgridfine(ix,iy,iz)%omega2))) problem=.true.
	   if (isnan(aimag(kgridfine(ix,iy,iz)%omega2))) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_points_reals_are_numbers &
              &[initialisation.fun:398]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine fine_points_reals_are_numbers


 subroutine fine_grid_kx_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kx
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_kx_within_range &
              &[initialisation.fun:423]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kx
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_kx_within_range &
              &[initialisation.fun:437]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine fine_grid_kx_within_range

  
 subroutine fine_grid_ky_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%ky
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_ky_within_range &
              &[initialisation.fun:462]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%ky
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_ky_within_range &
              &[initialisation.fun:476]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine fine_grid_ky_within_range


 subroutine fine_grid_kz_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kz
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_kz_within_range &
              &[initialisation.fun:501]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kz
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_kz_within_range &
              &[initialisation.fun:515]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine fine_grid_kz_within_range


 subroutine fine_grid_knorm_within_range

  real(real12),parameter::upper=sqrt(27.0_real12*pi*pi/4.0_real12)
  real(real12),parameter::lower=0.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%norm
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_knorm_within_range &
              &[initialisation.fun:540]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%norm
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test fine_grid_knorm_within_range &
              &[initialisation.fun:554]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine fine_grid_knorm_within_range


!!!!!!!!!!!!!!!!!!!
 subroutine Coarse_points_reals_are_numbers

  logical::problem
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kx)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:576]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%ky)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:589]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:602]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
	   
  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:615]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%norm)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:628]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(real(kgridCoarse(ix,iy,iz)%omega2))) problem=.true.
	   if (isnan(aimag(kgridCoarse(ix,iy,iz)%omega2))) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_points_reals_are_numbers &
              &[initialisation.fun:642]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine Coarse_points_reals_are_numbers


 subroutine Coarse_grid_kx_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kx
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_kx_within_range &
              &[initialisation.fun:667]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kx
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_kx_within_range &
              &[initialisation.fun:681]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine Coarse_grid_kx_within_range

  
 subroutine Coarse_grid_ky_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%ky
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_ky_within_range &
              &[initialisation.fun:706]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%ky
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_ky_within_range &
              &[initialisation.fun:720]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine Coarse_grid_ky_within_range


 subroutine Coarse_grid_kz_within_range

  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kz
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_kz_within_range &
              &[initialisation.fun:745]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kz
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_kz_within_range &
              &[initialisation.fun:759]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine Coarse_grid_kz_within_range


 subroutine Coarse_grid_knorm_within_range

  real(real12),parameter::upper=sqrt(27.0_real12*pi*pi/4.0_real12)
  real(real12),parameter::lower=0.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%norm
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_knorm_within_range &
              &[initialisation.fun:784]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%norm
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (problem) then
      print *, " *Assert_False failed* in test Coarse_grid_knorm_within_range &
              &[initialisation.fun:798]"
      print *, "  ", "problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  

  numTests = numTests + 1

 end subroutine Coarse_grid_knorm_within_range


 subroutine funit_setup
  !code that should be run before each test
  settings%nfpoints=fine_points
  settings%ncell=ncoarse_cells
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown
  !code runs after each test
  integer::stat

  if (allocated(kgridFine)) deallocate(kgridFine)
  if (allocated(kgridCoarse)) deallocate(kgridCoarse)
 end subroutine funit_teardown


 subroutine test_initialisation( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call negative_finegrid_ierr
  call funit_teardown

  call funit_setup
  call odd_finegrid_ierr
  call funit_teardown

  call funit_setup
  call not_allocatated_fine_grid_ierr
  call funit_teardown

  call funit_setup
  call not_allocatated_coarse_grid_ierr
  call funit_teardown

  call funit_setup
  call negative_coarsegrid_ierr
  call funit_teardown

  call funit_setup
  call odd_coarsegrid_ierr
  call funit_teardown

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

  call funit_setup
  call fine_points_reals_are_numbers
  call funit_teardown

  call funit_setup
  call fine_grid_kx_within_range
  call funit_teardown

  call funit_setup
  call fine_grid_ky_within_range
  call funit_teardown

  call funit_setup
  call fine_grid_kz_within_range
  call funit_teardown

  call funit_setup
  call fine_grid_knorm_within_range
  call funit_teardown

  call funit_setup
  call Coarse_points_reals_are_numbers
  call funit_teardown

  call funit_setup
  call Coarse_grid_kx_within_range
  call funit_teardown

  call funit_setup
  call Coarse_grid_ky_within_range
  call funit_teardown

  call funit_setup
  call Coarse_grid_kz_within_range
  call funit_teardown

  call funit_setup
  call Coarse_grid_knorm_within_range
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_initialisation

end module initialisation_fun
