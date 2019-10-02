test_suite dispersions

!Global variables

setup

end setup

teardown

end teardown

test fine_disp_zero
     real(real12) :: test
     real(real12) :: zero = 0.0_real12
     real(real12) :: tolerance = epsilon(test)

     test=fineDispersion(zero, zero, zero)
     assert_equal_within(test, zero, tolerance)
end test

end test_suite