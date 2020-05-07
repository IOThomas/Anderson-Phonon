test_suite gf_fourier

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

setup

 call allocateGF(greens_function, x_size, y_size, z_size, n_omega, ierr)
 ierr=0
end setup

teardown

  if (allocated(greens_function)) deallocate(greens_function)

end teardown

test initialise_kill_cycle
     logical :: init_plan
     logical :: forward_associated, backward_associated
     logical :: work_allocated, forward_null, backward_null

     !calls the plan initialisation routines and then kills the plan
     !checks plan status before, during and after 

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

     assert_false(init_plan)
     assert_false(forward_associated)
     assert_false(backward_associated)
     assert_false(work_allocated)
     assert_false(forward_null)
     assert_false(backward_null)

     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

     assert_true(init_plan)
     assert_true(forward_associated)
     assert_true(backward_associated)
     assert_true(work_allocated)
     assert_false(forward_null)
     assert_false(backward_null)

     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

     call get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)

     assert_false(init_plan)
     assert_true(forward_associated)
     assert_true(backward_associated)
     assert_false(work_allocated)
     assert_false(forward_null)
     assert_false(backward_null)
end test

test kill_without_initialisation

     ! checks that error issued if plan is killed before it's initialised
     call greensfunc_killplan(ierr)
     assert_equal(ierr,1)
     
end test

test intialise_twice

     ! checks plan can't be initialised twice, then kills plan
     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)
     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,1)
     
     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

end test

test fft_without_initialisation

     !checks you can't run the FFT without initialising a plan
     call gf_fft(greens_function, forward_fft, ierr)
     assert_equal(ierr,1)
     
end test

test fft_wrong_transform_flag

     !checks an FFT can't be called if the type of transform
     ! is set to an invalid value
     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)

     call gf_fft(greens_function, 0, ierr)
     assert_equal(ierr,2)

     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

end test

test fft_array_too_big
     type(greensfunc), allocatable:: test(:, :, :)
     integer :: too_big = 300

     !checks that errors are returned if array sizes different from those
     !in the plan are passed to the FFT
     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)

     call allocateGF(test, too_big, y_size, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,3)
     deallocate(test)

     call allocateGF(test, x_size, too_big, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,4)
     deallocate(test)

     call allocateGF(test, x_size, y_size, too_big, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,5)
     deallocate(test)

     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)
end test

test fft_array_too_small
     type(greensfunc), allocatable:: test(:, :, :)
     integer :: too_small = 1

     !checks that errors are returned if array sizes different from those
     !in the plan are passed to the FFT

     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)

     call allocateGF(test, too_small, y_size, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,3)
     deallocate(test)

     call allocateGF(test, x_size, too_small, z_size, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,4)
     deallocate(test)

     call allocateGF(test, x_size, y_size, too_small, n_omega, ierr)
     call gf_fft(test, forward_fft, ierr)
     assert_equal(ierr,5)
     deallocate(test)

     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)
end test

test forward_then_back_real
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
     assert_equal(ierr,0)
     call gf_fft(greens_function, forward_fft, ierr)
     assert_equal(ierr,0)

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
     assert_equal(ierr,0)

     
     assert_false(real_inter_prob)
     assert_false(imag_inter_prob)

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

     assert_false(real_final_prob)
     assert_false(imag_final_prob)
     
     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

     do i = 1, x_size
     	do j = 1, y_size
	   do k = 1, z_size
	      deallocate(initial_gf(i, j, k)%GF)
              deallocate(intermediate_gf(i, j, k)%GF)
	   enddo
	enddo
     enddo

end test
end test_suite
