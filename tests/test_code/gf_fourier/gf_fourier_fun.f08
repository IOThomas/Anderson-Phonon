! gf_fourier_fun.f90 - a unit test suite for gf_fourier.f90
!
! funit generated this file from gf_fourier.fun

module gf_fourier_fun

 use constants
 use definedtypes
 use greensroutines
 use gf_test_routines
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
  integer, parameter		  :: x_size = 21
  integer, parameter              :: y_size = 31
  integer, parameter              :: z_size = 41
  integer, parameter              :: x_by_y = x_size*y_size
  integer, parameter              :: y_by_z = y_size*z_size
  integer, parameter              :: z_by_x = z_size*x_size
  real(real12), parameter         :: x_length = real(x_size, real12)
  real(real12), parameter         :: y_length = real(y_size, real12)
  real(real12), parameter         :: z_length = real(z_size, real12)
  integer, parameter              :: n_omega = 3
  integer                         :: ierr = 0
  type(greensfunc), allocatable   :: greens_function(:, :, :)

 contains



 subroutine initialise_kill_cycle

     logical :: init_plan
     logical :: forward_associated, backward_associated
     logical :: work_allocated, forward_null, backward_null

     !calls the plan initialisation routines and then kills the plan
     !checks plan status before, during and after 

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (init_plan) then
      print *, " *Assert_False failed* in test initialise_kill_cycle &
              &[gf_fourier.fun:40]"
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
              &[gf_fourier.fun:41]"
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
              &[gf_fourier.fun:42]"
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
              &[gf_fourier.fun:43]"
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
              &[gf_fourier.fun:44]"
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
              &[gf_fourier.fun:45]"
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
              &[gf_fourier.fun:48]"
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
              &[gf_fourier.fun:53]"
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
              &[gf_fourier.fun:54]"
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
              &[gf_fourier.fun:55]"
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
              &[gf_fourier.fun:56]"
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
              &[gf_fourier.fun:57]"
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
              &[gf_fourier.fun:58]"
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
              &[gf_fourier.fun:61]"
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
              &[gf_fourier.fun:66]"
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
              &[gf_fourier.fun:67]"
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
              &[gf_fourier.fun:68]"
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
              &[gf_fourier.fun:69]"
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
              &[gf_fourier.fun:70]"
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
              &[gf_fourier.fun:71]"
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


     ! checks that error issued if plan is killed before it's initialised
     call greensfunc_killplan(ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test kill_without_initialisation &
              &[gf_fourier.fun:78]"
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


     ! checks plan can't be initialised twice, then kills plan
     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test intialise_twice &
              &[gf_fourier.fun:86]"
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
              &[gf_fourier.fun:88]"
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
              &[gf_fourier.fun:91]"
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


     !checks you can't run the FFT without initialising a plan
     call gf_fft(greens_function, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==1)) then
      print *, " *Assert_Equal failed* in test fft_without_initialisation &
              &[gf_fourier.fun:99]"
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


     !checks an FFT can't be called if the type of transform
     ! is set to an invalid value
     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_wrong_transform_flag &
              &[gf_fourier.fun:108]"
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
              &[gf_fourier.fun:111]"
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
              &[gf_fourier.fun:114]"
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
     integer :: too_big = 300

     !checks that errors are returned if array sizes different from those
     !in the plan are passed to the FFT
     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_big &
              &[gf_fourier.fun:125]"
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
              &[gf_fourier.fun:129]"
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
              &[gf_fourier.fun:134]"
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
              &[gf_fourier.fun:139]"
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
              &[gf_fourier.fun:143]"
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

     !checks that errors are returned if array sizes different from those
     !in the plan are passed to the FFT

     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test fft_array_too_small &
              &[gf_fourier.fun:154]"
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
              &[gf_fourier.fun:158]"
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
              &[gf_fourier.fun:163]"
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
              &[gf_fourier.fun:168]"
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
              &[gf_fourier.fun:172]"
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


 subroutine forward_then_back_real

     type(greensfunc) :: initial_gf(x_size, y_size, z_size)
     type(greensfunc) :: intermediate_gf(x_size, y_size, z_size)
     integer          :: i, j, k, l, ierr
     real(real12)     :: test_value, target_value
     real(real12)     :: real_comp, imag_comp, angle
     real(real12), parameter :: alpha = 0.5_real12
     complex(real12)  :: start, intermediate, phase, denom
     logical          :: real_inter_prob = .false.
     logical          :: imag_inter_prob = .false.
     logical          :: real_final_prob = .false.
     logical          :: imag_final_prob = .false.
     logical          :: is_problem = .false.

     !checks that the FFT can transform a known function and back again
     !initialise test values
     do i = 1, x_size
     	do j = 1, y_size
	   do k = 1, z_size
	      allocate(initial_gf(i, j, k)%GF(n_omega))
              allocate(intermediate_gf(i, j, k)%GF(n_omega))
	      start = initial_function(alpha, i, j, k)
       
              intermediate=intermediate_function(alpha, i, x_length, start)* &
                   & intermediate_function(alpha, j, y_length, start)* &
                   & intermediate_function(alpha, k, z_length, start)
	      
	      do l = 1, n_omega
	      	 initial_gf(i, j, k)%GF(l) = start
		 greens_function(i, j, k)%GF(l) = start
		 intermediate_gf(i, j, k)%GF(l) = intermediate
		 test_value=real(greens_function(i, j, k)%GF(l))
		 target_value=start
		 write(10,*) i, j, k, l, test_value, target_value, (test_value-target_value)
	      enddo
	   enddo
	enddo
     enddo

     call greensfunc_initplan(greens_function, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test forward_then_back_real &
              &[gf_fourier.fun:215]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     call gf_fft(greens_function, forward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test forward_then_back_real &
              &[gf_fourier.fun:217]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     do i = 1, x_size
     	if (is_problem) exit
     	do j = 1, y_size
	   if (is_problem) exit
	   do k = 1, z_size
	      if (is_problem) exit
	      do l = 1, n_omega
	      	 test_value=real(greens_function(i, j, k)%GF(l))
		 target_value=real(intermediate_gf(i, j, k)%GF(l))
		
	      	 !if (test_value.ne.target_value) then
		 if (abs(test_value-target_value).gt.tolerance) then
		    real_inter_prob = .true.
		    is_problem=.true.
		    exit
		 endif

		 test_value=aimag(greens_function(i, j, k)%GF(l))
		 target_value=aimag(intermediate_gf(i, j, k)%GF(l))
	      	 !if (test_value.ne.target_value) then
		 if (abs(test_value-target_value).gt.tolerance) then
		    imag_inter_prob = .true.
		    is_problem=.true.
		    exit
		 endif
	      enddo
	   enddo
	enddo
     enddo

     call gf_fft(greens_function, backward_fft, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test forward_then_back_real &
              &[gf_fourier.fun:250]"
      print *, "  ", "ierr (",ierr,") is not", 0
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
    if (real_inter_prob) then
      print *, " *Assert_False failed* in test forward_then_back_real &
              &[gf_fourier.fun:253]"
      print *, "  ", "real_inter_prob is not false"
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
    if (imag_inter_prob) then
      print *, " *Assert_False failed* in test forward_then_back_real &
              &[gf_fourier.fun:254]"
      print *, "  ", "imag_inter_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     do i = 1, x_size
     	if (is_problem) exit
     	do j = 1, y_size
	   if (is_problem) exit
	   do k = 1, z_size
	      if (is_problem) exit
	      do l = 1, n_omega
	      	 test_value=real(greens_function(i, j, k)%GF(l))
		 target_value=real(initial_gf(i, j, k)%GF(l))
	      	 !if (test_value.ne.target_value) then
		 if (abs(test_value-target_value).gt.tolerance) then
		    real_final_prob = .true.
		    is_problem=.true.
		    exit
		 endif

		 test_value=aimag(greens_function(i, j, k)%GF(l))
		 target_value=aimag(initial_gf(i, j, k)%GF(l))
	      	 !if (test_value.ne.target_value) then
		 if (abs(test_value-target_value).gt.tolerance) then
		    imag_final_prob = .true.
		    is_problem=.true.
		    exit
		 endif
	      enddo
	   enddo
	enddo
     enddo

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (real_final_prob) then
      print *, " *Assert_False failed* in test forward_then_back_real &
              &[gf_fourier.fun:285]"
      print *, "  ", "real_final_prob is not false"
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
    if (imag_final_prob) then
      print *, " *Assert_False failed* in test forward_then_back_real &
              &[gf_fourier.fun:286]"
      print *, "  ", "imag_final_prob is not false"
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
      print *, " *Assert_Equal failed* in test forward_then_back_real &
              &[gf_fourier.fun:289]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     do i = 1, x_size
     	do j = 1, y_size
	   do k = 1, z_size
	      deallocate(initial_gf(i, j, k)%GF)
              deallocate(intermediate_gf(i, j, k)%GF)
	   enddo
	enddo
     enddo


  numTests = numTests + 1

 end subroutine forward_then_back_real


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

  call funit_setup
  call forward_then_back_real
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_gf_fourier

end module gf_fourier_fun
