test_suite dispersions


setup

end setup

teardown

end teardown

test fine_disp_zero
     complex(real12)         :: test    

     test = fineDispersion(zero, zero, zero)
     assert_equal_within(real(test), zero, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)
end test

test fine_max_kx
     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(input, zero, zero)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)
end test

test fine_max_ky
     complex(real12)          :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(zero, input, zero)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)
end test

test fine_max_kz
     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(zero, zero, input)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)
end test

test fine_max_kall
     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = three

     test = fineDispersion(input, input, input)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)
end test

test fine_periodicty_kx
     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(input1, zero, zero)
     test2 = fineDispersion(input2, zero, zero)
     assert_equal_within(real(test1),real(test2), tolerance)
     assert_equal_within(aimag(test1), aimag(test2), tolerance)
end test

test fine_periodicty_ky
     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(zero, input1 , zero)
     test2 = fineDispersion(zero, input1 , zero)
     assert_equal_within(real(test1),real(test2), tolerance)
     assert_equal_within(aimag(test1), aimag(test2), tolerance)
end test

test fine_periodicty_kz
     complex(real12)            :: test1
     complex(real12)            :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(zero, zero, input1)
     test2 = fineDispersion(zero, zero, input2)
     assert_equal_within(real(test1),real(test2), tolerance)
     assert_equal_within(aimag(test1), aimag(test2), tolerance)
end test

test fine_periodicty_kall
     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(input1, input1, input1)
     test2 = fineDispersion(input2, input2, input2)
     assert_equal_within(real(test1),real(test2), tolerance)
     assert_equal_within(aimag(test1), aimag(test2), tolerance)
end test

test coarse_summation_someones
     integer, parameter      :: ifinecell = 4
     integer, parameter      :: ifinetot = ifinecell**3
     integer, parameter      :: icoarsetot = 8
     complex(real12)         :: testgrid(ifinecell, ifinecell, ifinecell)
     complex(real12)         :: test
     real(real12), parameter :: nonzero_vals = three
     real(real12), parameter :: ratio = real(icoarsetot) / real(ifinetot)
     real(real12), parameter :: expected = nonzero_vals * ratio

     testgrid=(zero, zero)
     testgrid(2, 1, 3) = (one, zero)
     testgrid(1, 4, 3) = (one, zero)
     testgrid(3, 1, 2) = (one, zero)

     test=coarseDispersion(testgrid, icoarsetot, ifinetot)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), zero, tolerance)     
end test

test coarse_summation_allzero
     integer, parameter      :: ifinecell = 4
     integer, parameter      :: ifinetot = ifinecell**3
     integer, parameter      :: icoarsetot = 8
     complex(real12)         :: testgrid(ifinecell, ifinecell, ifinecell)
     complex(real12)         :: test
     real(real12), parameter :: nonzero_vals = zero
     real(real12), parameter :: ratio = real(icoarsetot) / real(ifinetot)
     real(real12), parameter :: expected = nonzero_vals * ratio

     testgrid=(zero, zero)

     test=coarseDispersion(testgrid, icoarsetot, ifinetot)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), expected, tolerance)     
end test

test coarse_summation_allone
     integer, parameter      :: ifinecell = 4
     integer, parameter      :: ifinetot = ifinecell**3
     integer, parameter      :: icoarsetot = 8
     complex(real12)         :: testgrid(ifinecell, ifinecell, ifinecell)
     complex(real12)         :: test
     real(real12), parameter :: nonzero_vals = real(ifinetot)
     real(real12), parameter :: ratio = real(icoarsetot) / real(ifinetot)
     real(real12), parameter :: expected = nonzero_vals * ratio

     testgrid=(one, one)


     test=coarseDispersion(testgrid, icoarsetot, ifinetot)
     assert_equal_within(real(test), expected, tolerance)
     assert_equal_within(aimag(test), expected, tolerance)     
end test

     


end test_suite