test_suite gf_fourier

! global variables
  integer, parameter		  :: x_size = 5
  integer, parameter              :: y_size = 10
  integer, parameter              :: z_size = 15
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

     call greensfunc_killplan(ierr)
     assert_equal(ierr,1)
     
end test

test intialise_twice

     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)
     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,1)
     
     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

end test

test fft_without_initialisation

     call gf_fft(greens_function, forward_fft, ierr)
     assert_equal(ierr,1)
     
end test

test fft_wrong_transform_flag

     call greensfunc_initplan(greens_function, ierr)
     assert_equal(ierr,0)

     call gf_fft(greens_function, 0, ierr)
     assert_equal(ierr,2)

     call greensfunc_killplan(ierr)
     assert_equal(ierr,0)

end test

test fft_array_too_big
     type(greensfunc), allocatable:: test(:, :, :)
     integer :: too_big = 20

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
     
end test_suite