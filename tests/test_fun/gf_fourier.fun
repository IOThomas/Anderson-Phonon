test_suite gf_fourier

! global variables
  integer, parameter		  :: x_size = 5
  integer, parameter              :: y_size = 10
  integer, parameter              :: z_size = 15
  integer, parameter              :: n_omega = 3
  integer                         :: ierr = 0
  type(greensfunc), allocatable   :: greens_function(:, :, :)

setup

 call allocateGF(greens_function, x_size, z_size, y_size, n_omega, ierr)
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


     


end test_suite