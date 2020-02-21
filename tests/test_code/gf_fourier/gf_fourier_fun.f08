! gf_fourier_fun.f90 - a unit test suite for gf_fourier.f90
!
! funit generated this file from gf_fourier.fun

module gf_fourier_fun

 use constants
 use greensroutines
 use gf_fourier

 implicit none

 logical :: noAssertFailed

 public :: test_gf_fourier

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



! global variables
  integer, parameter		  :: x_size = 5
  integer, parameter              :: y_size = 10
  integer, parameter              :: z_size = 15
  integer, parameter              :: n_omega = 3
  integer                         :: ierr = 0
  type(greensfunc), allocatable   :: greens_function(:, :, :)

 contains



 subroutine initialise_kill_cycle

     logical :: init_plan
     logical :: forward_associated, backward_associated
     logical :: work_allocated, forward_null, backward_null

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (init_plan) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:31]"
      print *, "  ", "init_plan is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (forward_associated) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:32]"
      print *, "  ", "forward_associated is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (backward_associated) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:33]"
      print *, "  ", "backward_associated is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (work_allocated) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:34]"
      print *, "  ", "work_allocated is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (forward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:35]"
      print *, "  ", "forward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (backward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:36]"
      print *, "  ", "backward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:39]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(init_plan)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:44]"
      print *, "  ", "init_plan is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(forward_associated)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:45]"
      print *, "  ", "forward_associated is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(backward_associated)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:46]"
      print *, "  ", "backward_associated is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(work_allocated)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:47]"
      print *, "  ", "work_allocated is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (forward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:48]"
      print *, "  ", "forward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (backward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:49]"
      print *, "  ", "backward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call greensfunc_killplan(ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:52]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (init_plan) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:57]"
      print *, "  ", "init_plan is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(forward_associated)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:58]"
      print *, "  ", "forward_associated is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(backward_associated)) then
      print *, " *Assert_True failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:59]"
      print *, "  ", "backward_associated is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (work_allocated) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:60]"
      print *, "  ", "work_allocated is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (forward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:61]"
      print *, "  ", "forward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (backward_null) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:62]"
      print *, "  ", "backward_null is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine initialise_kill_cycle



     

 subroutine funit_setup

 call allocateGF(greens_function, x_size, z_size, y_size, n_omega, ierr)
 ierr=0
  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown

  if (allocated(greens_function)) deallocate(greens_function)

 end subroutine funit_teardown


 subroutine test_gf_fourier( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call initialise_kill_cycle
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_gf_fourier

end module gf_fourier_fun
