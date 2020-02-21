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


 subroutine kill_without_initialisation


     call greensfunc_killplan(ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test kill_without_initialisation &
              &[gf_fourier.fun:68]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

  numTests = numTests + 1

 end subroutine kill_without_initialisation


 subroutine intialise_twice


     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test intialise_twice &
              &[gf_fourier.fun:75]"
      print *, "  ", "ierr (",ierr,") is not", 0
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
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test intialise_twice &
              &[gf_fourier.fun:77]"
      print *, "  ", "ierr (",ierr,") is not", 1
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
      print *, " *Assert_Equal failed* in test intialise_twice &
              &[gf_fourier.fun:80]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine intialise_twice


 subroutine fft_without_initialisation


     call gf_fft(greens_function, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test fft_without_initialisation &
              &[gf_fourier.fun:87]"
      print *, "  ", "ierr (",ierr,") is not", 1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

  numTests = numTests + 1

 end subroutine fft_without_initialisation


 subroutine fft_wrong_transform_flag


     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_wrong_transform_flag &
              &[gf_fourier.fun:94]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call gf_fft(greens_function, 0, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==2)) then
      print *, " *Assert_Equal failed* in test fft_wrong_transform_flag &
              &[gf_fourier.fun:97]"
      print *, "  ", "ierr (",ierr,") is not", 2
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
      print *, " *Assert_Equal failed* in test fft_wrong_transform_flag &
              &[gf_fourier.fun:100]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine fft_wrong_transform_flag


 subroutine fft_array_too_big

     type(greensfunc), allocatable:: test(:, :, :)
     integer :: too_big = 20

     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:109]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(test, too_big, y_size, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==3)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:113]"
      print *, "  ", "ierr (",ierr,") is not", 3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call allocateGF(test, x_size, too_big, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==4)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:118]"
      print *, "  ", "ierr (",ierr,") is not", 4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call allocateGF(test, x_size, y_size, too_big, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==5)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:123]"
      print *, "  ", "ierr (",ierr,") is not", 5
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call greensfunc_killplan(ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:127]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fft_array_too_big


 subroutine fft_array_too_small

     type(greensfunc), allocatable:: test(:, :, :)
     integer :: too_small = 1

     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:135]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(test, too_small, y_size, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==3)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:139]"
      print *, "  ", "ierr (",ierr,") is not", 3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call allocateGF(test, x_size, too_small, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==4)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:144]"
      print *, "  ", "ierr (",ierr,") is not", 4
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call allocateGF(test, x_size, y_size, too_small, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==5)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:149]"
      print *, "  ", "ierr (",ierr,") is not", 5
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(test)

     call greensfunc_killplan(ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:153]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fft_array_too_small

     

 subroutine funit_setup

 call allocateGF(greens_function, x_size, y_size, z_size, n_omega, ierr)
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

  call funit_setup
  call kill_without_initialisation
  call funit_teardown

  call funit_setup
  call intialise_twice
  call funit_teardown

  call funit_setup
  call fft_without_initialisation
  call funit_teardown

  call funit_setup
  call fft_wrong_transform_flag
  call funit_teardown

  call funit_setup
  call fft_array_too_big
  call funit_teardown

  call funit_setup
  call fft_array_too_small
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_gf_fourier

end module gf_fourier_fun
