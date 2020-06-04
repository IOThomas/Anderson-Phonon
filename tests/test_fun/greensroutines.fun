test_suite greensroutines

setup

end setup

teardown

end teardown

test allocateGF_sizes
     integer, parameter            :: array_size = 2
     type(greensfunc), allocatable :: testGF(:, :, :)
     logical                       :: GFsize_prob = .false.
     logical                       :: get_size_prob = .false.
     integer                       :: i, j, k, itest, igettest

     allocate(testGF(array_size, array_size, array_size))
     call allocate_GF(testGF, array_size)

     assert_equal(size(testGF, 1), array_size)
     assert_equal(size(testGF, 2), array_size)
     assert_equal(size(testGF, 3), array_size)


     do i = 1, array_size
     	if (GFsize_prob.or.get_size_prob) exit
     	do j = 1, array_size
	   if (GFsize_prob.or.get_size_prob) exit
           do k = 1, array_size
              if (GFsize_prob.or.get_size_prob) exit
              
	      itest = size(testGF(i, j, k)%GF, 1)
              if (itest.ne.array_size) GFsize_prob = .true.
              igettest=testGF(i, j, k)%get_size()
              if (itest.ne.igettest) get_size_prob = .true.
	   enddo
	enddo
      enddo

      assert_false(GFsize_prob)
      assert_false(get_size_prob)
      deallocate(testGF)
end test


test calculateGF_output_withhyb
     type(greensfunc)           :: testGF(2, 2, 2)
     type(greensfunc)           :: testHyb(2, 2, 2)
     type(kappagrid)            :: testdisp(2, 2, 2)
     integer, parameter         :: array_size = 2
     integer                    :: i, j , k, l, ierr
     real(real12), parameter    :: Hybval = 1.0_real12
     real(real12), parameter    :: Dispval = 1.0_real12
     complex(real12), parameter :: Deltamom = (2.0_real12, zero)
     real(real12), parameter    :: test_val1 = one/2.0_real12
     real(real12), parameter    :: test_val2 = one/14.0_real12
     logical                    :: outwh_prob = .false.

     
     do i = 1, array_size
     	do j = 1, array_size
	   do k = 1, array_size
	   testdisp(i, j, k)%omega2 = Dispval
	   allocate(testGF(i, j, k)%GF(array_size))
	   allocate(testHyb(i, j, k)%GF(array_size))
	       do l=1, array_size
	       	  testHyb(i, j, k)%GF(l) = Hybval
	       enddo
	   enddo
	enddo
      enddo

     call calculateGF(testGF, deltamom, testdisp, testhyb, ierr)
     assert_equal(ierr, 0)

     do i = 1, array_size
     	if (outwh_prob) then
	   exit
	endif
     	do j = 1, array_size
	   if (outwh_prob) then
	      exit
	   endif	
	   do k = 1, array_size
	      if (outwh_prob) then
	      	 exit
	       endif
	       do l = 1, array_size
	           if (((real(testGF(i, j, k)%GF(l)).ne.test_val1).and.&
		      (l.eq.1)).or.&
		      ((real(testGF(i, j, k)%GF(l)).ne.test_val2).and.&
		      (l.eq.2))) then
	      	       outwh_prob = .true.
		       exit
	           end if

		   if (aimag(testGF(i, j, k)%GF(l)).ne.zero) then
	      	       outwh_prob = .true.
		       exit
	           end if
	       enddo
	   enddo
	enddo
      enddo

      assert_false(outwh_prob)

end test

test calculateGF_output_withouthyb
     type(greensfunc)           :: testGF(2, 2, 2)
     type(greensfunc)           :: testHyb(2, 2, 2)
     type(kappagrid)            :: testdisp(2, 2, 2)
     integer, parameter         :: array_size = 2
     integer                    :: i, j , k, l, ierr
     real(real12), parameter    :: Dispval = 1.0_real12
     complex(real12), parameter :: Deltamom = (2.0_real12, zero)
     real(real12), parameter    :: test_val1 = one/3.0_real12
     real(real12), parameter    :: test_val2 = one/15.0_real12
     logical                    :: outnoh_prob = .false.

     
     do i = 1, array_size
     	do j = 1, array_size
	   do k = 1, array_size
	   testdisp(i, j, k)%omega2 = Dispval
	   allocate(testGF(i, j, k)%GF(array_size))
	enddo
      enddo
     enddo

     call calculateGF(GFval = testGF, deltaw = deltamom, &
         dispersion = testdisp, ierr = ierr)
     assert_equal(ierr,0)

     do i = 1, array_size
     	if (outnoh_prob) exit
     	do j = 1, array_size
	   if (outnoh_prob) exit
	   do k = 1, array_size
	      if (outnoh_prob) exit
	       do l = 1, array_size
	           if (((real(testGF(i, j, k)%GF(l)).ne.test_val1).and.&
		      (l.eq.1)).or.&
		      ((real(testGF(i, j, k)%GF(l)).ne.test_val2).and.&
		      (l.eq.2))) then
	      	       outnoh_prob = .true.
		       exit
	           end if
		  
		   if (aimag(testGF(i, j, k)%GF(l)).ne.zero) then
		       outnoh_prob = .true.
		       exit
	           end if
	       enddo
	   enddo
	enddo
      enddo

      assert_false(outnoh_prob)

end test

test calculateGF_divbyzero
     type(greensfunc)           :: testGF(2, 2, 2)
     type(greensfunc)           :: testHyb(2, 2, 2)
     type(kappagrid)            :: testdisp(2, 2, 2)
     integer, parameter         :: array_size = 2
     integer                    :: i, j , k, l, ierr
     real(real12), parameter    :: Hybval = 1.0_real12
     real(real12), parameter    :: Dispval = 1.0_real12
     complex(real12), parameter :: Deltamom = 2.0_real12
     real(real12), parameter    :: test_val1 = 2.0_real12
     real(real12), parameter    :: test_val2 = 14.0_real12
     logical                    :: outwh_prob = .false.

     
     do i = 1, array_size
     	do j = 1, array_size
	   do k = 1, array_size
	   testdisp(i, j, k)%omega2 = Dispval
	   allocate(testGF(i, j, k)%GF(array_size))
	   allocate(testHyb(i, j, k)%GF(array_size))
	       do l=1, array_size
	       	  testHyb(i, j, k)%GF(l) = Hybval
	       enddo
	   enddo
	enddo
     enddo
     
     !set inputs so have division zero at a given point
     testdisp(2, 1, 2)%omega2 = 2.0_real12
     testHyb(2, 1, 2)%GF(1) = 2.0_real12

     call calculateGF(testGF, deltamom, testdisp, testhyb, ierr)
     assert_equal(ierr, 1)

end test

test GF_copying_individual_components
     type(greensfunc), allocatable :: original(:, :, :)
     type(greensfunc), allocatable :: copy(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     integer                       :: i, j, k, l, ierr
     logical                       :: copymap_prob = .false.
     logical                       :: copyGF_prob = .false.

     allocate(original(arraysize, arraysize, arraysize))
     call allocate_GF(original, arraysize)

     allocate(copy(arraysize, arraysize, arraysize))
     call allocate_GF(copy, arraysize)

     
     do i = 1, arraysize
     	do j = 1, arraysize
	   do k = 1, arraysize
	      original(i, j, k)%map = testmap
	      do l = 1, arraysize
		 original(i, j, k)%GF(l) = cmplx(testGF, testGF)
	      enddo
	   enddo
	enddo
     enddo

     call copymap(copy, original)
     call copyGF(copy, original)
     
     do i = 1, arraysize
        if (copyGF_prob.and.copymap_prob) exit
     	do j = 1, arraysize
	   if (copyGF_prob.and.copymap_prob) exit
	   do k = 1, arraysize
	      if (copyGF_prob.and.copymap_prob) exit
	      if (original(i, j, k)%map /= testmap) copymap_prob = .true.
	      do l = 1, arraysize
	         if (copyGF_prob.and.copymap_prob) exit
		 if (real(original(i, j, k)%GF(l)) /= testGF) &
		     copyGF_prob = .true.
		 if (aimag(original(i, j, k)%GF(l)) /= testGF) &
		     copyGF_prob = .true.
	      enddo
	   enddo
	enddo
     enddo

     assert_false(copyGF_prob)
     assert_false(copymap_prob)
     deallocate(original, copy)
end test

test GF_invert
     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     real(real12), parameter       :: GFans = 0.5_real12
     integer                       :: i, j, k, l, ierr
     integer, allocatable          :: ierr1(:, :, :)
     logical                       :: invertGF_prob = .false.

     allocate(test(arraysize, arraysize, arraysize))
     call allocate_GF(test,arraysize)

     allocate(ierr1(arraysize, arraysize, arraysize))
     
     do i = 1, arraysize
     	do j = 1, arraysize
	   do k = 1, arraysize
	      test(i, j, k)%map = testmap
	      do l = 1, arraysize
		 test(i, j, k)%GF(l) = cmplx(testGF, zero)
	      enddo
	   enddo
	enddo
     enddo

     call invertGF(test, ierr1)
     assert_true(all(ierr1 ==  0))
     
     do i = 1, arraysize
        if (invertGF_prob) exit
     	do j = 1, arraysize
	   if (invertGF_prob) exit
	   do k = 1, arraysize
	      if (invertGF_prob) exit
              do l = 1, arraysize
	      	 if (invertGF_prob) exit
		 if (real(test(i, j, k)%GF(l)) /= GFans) &
		     invertGF_prob = .true.
		 if (aimag(test(i, j, k)%GF(l)) /= zero) &
		     invertGF_prob = .true.
	      enddo
	   enddo
	enddo
     enddo

     assert_false(invertGF_prob)
     
     deallocate(test, ierr1)
end test

test GFinvert_ierr_divby0
     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     integer                       :: i, j, k, l, ierr
     integer, allocatable          :: ierr_invertGF(:, :, :)

     allocate(test(arraysize, arraysize, arraysize))
     call allocate_GF(test, arraysize)

     allocate(ierr_invertGF(arraysize, arraysize, arraysize))
     
     do i = 1, arraysize
     	do j = 1, arraysize
	   do k = 1, arraysize
	      test(i, j, k)%map = testmap
	      do l = 1, arraysize
		 test(i, j, k)%GF(l) = cmplx(testGF, zero)
	      enddo
	   enddo
	enddo
     enddo

     test(2, 1, 1)%GF(2) = zero

     call invertGF(test, ierr_invertGF)
     assert_true(any(ierr_invertGF == 1))

     deallocate(test)
end test

test GF_reduction
     type(greensfunc), allocatable :: coarse(:, :, :)
     type(greensfunc), allocatable :: fine(:, :, :)
     integer, parameter            :: coarse_size = 2
     integer, parameter            :: fine_size = 4
     integer, parameter            :: nomega = 2
     real(real12), parameter       :: test_input = one
     real(real12), parameter       :: test_output = 8.0_real12
     integer                       :: i, j, k, l, ierr
     integer                       :: ic, jc, kc
     logical                       :: reduce_problem = .false.

     allocate(coarse(coarse_size, coarse_size, coarse_size))
     call allocate_GF(coarse, nomega)

     allocate(fine(fine_size, fine_size, fine_size))
     call allocate_GF(fine, nomega)

     do ic = 1, coarse_size
     	do jc = 1, coarse_size
	   do kc = 1, coarse_size
	      coarse(ic , jc , kc)%map = ic + (jc - 1)*coarse_size &
	          + (kc - 1)*coarse_size*coarse_size
           enddo
	enddo
     enddo

     do i = 1, fine_size
     	do j = 1, fine_size
	   do k = 1, fine_size
	      ic = nint(real(i)/two)
	      jc = nint(real(j)/two)
	      kc = nint(real(k)/two)
	      fine(i , j , k)%map = ic + (jc - 1)*coarse_size &
	          + (kc - 1)*coarse_size*coarse_size
	      do l = 1, nomega
	      	 fine(i, j, k)%GF(l) = cmplx(one, one)
	      enddo
	   enddo
	enddo
     enddo

     call reduceGF(coarse, fine, ierr)
     assert_equal(ierr, 0)

     do i = 1, coarse_size
     	if (reduce_problem) exit
     	do j = 1, coarse_size
	   if (reduce_problem) exit
	   do k = 1, coarse_size
	      if (reduce_problem) exit
	      do l = 1, nomega
	      	 if (reduce_problem) exit
		 if (real(coarse(i, j, k)%GF(l)).ne.test_output) then
		    reduce_problem = .true. 
		    exit
		 endif
		 if (aimag(coarse(i, j, k)%GF(l)).ne.test_output) then
		    reduce_problem = .true.
		    exit
		 endif
	      enddo
	   enddo
        enddo
     enddo

     assert_false(reduce_problem)

end test

test reduceGF_ierr1     
     type(greensfunc), allocatable :: coarse(:, :, :)
     type(greensfunc), allocatable :: fine(:, :, :)
     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: coarse_size = 2
     integer, parameter            :: fine_size = 4
     integer, parameter            :: nomega = 2
     integer                       :: ierr

     allocate(coarse(coarse_size, coarse_size, coarse_size))
     call allocate_GF(coarse, nomega)

     allocate(fine(fine_size, fine_size, fine_size))
     call allocate_GF(fine, nomega)
   

     allocate(test(1, 1, 1))
     
     call reduceGF(coarse, test, ierr)
     assert_equal(ierr, 1)

     call reduceGF(test, fine, ierr)
     assert_equal(ierr, 1)
end test

test reduceGF_ierr2
     type(greensfunc), allocatable :: coarse(:, :, :)
     type(greensfunc), allocatable :: fine(:, :, :)
     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: coarse_size = 2
     integer, parameter            :: fine_size = 4
     integer, parameter            :: nomega = 2
     integer, parameter            :: nomega1 = 1
     integer                       :: ierr

     allocate(coarse(coarse_size, coarse_size, coarse_size))
     call allocate_GF(coarse, nomega)

     allocate(fine(fine_size, fine_size, fine_size))
     call allocate_GF(fine, nomega1)

     call reduceGF(coarse, fine, ierr)
     assert_equal(ierr, 2)
end test

test reduceGF_ierr3
     type(greensfunc), allocatable :: coarse(:, :, :)
     type(greensfunc), allocatable :: fine(:, :, :)
     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: coarse_size = 8
     integer, parameter            :: fine_size = 4
     integer, parameter            :: nomega = 2
     integer                       :: ierr

     allocate(coarse(coarse_size, coarse_size, coarse_size))
     call allocate_GF(coarse, nomega)

     allocate(fine(fine_size, fine_size, fine_size))
     call allocate_GF(fine, nomega)

     call reduceGF(coarse, fine, ierr)
     assert_equal(ierr, 3)
end test

test GF_overloaded_equals
     type(greensfunc), allocatable :: original(:, :, :)
     type(greensfunc), allocatable :: copy(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     integer                       :: i, j, k, l, ierr
     logical                       :: copymap_prob = .false.
     logical                       :: copyGF_prob = .false.

     allocate(original(arraysize, arraysize, arraysize))
     call allocate_GF(original, arraysize)

     allocate(copy(arraysize, arraysize, arraysize))
     call allocate_GF(copy, arraysize)

     
     do i = 1, arraysize
     	do j = 1, arraysize
	   do k = 1, arraysize
	      original(i, j, k)%map = testmap
	      do l = 1, arraysize
		 original(i, j, k)%GF(l) = cmplx(testGF, testGF)
	      enddo
	   enddo
	enddo
     enddo

     copy = original
     
     do i = 1, arraysize
        if (copyGF_prob.and.copymap_prob) exit
     	do j = 1, arraysize
	   if (copyGF_prob.and.copymap_prob) exit
	   do k = 1, arraysize
	      if (copyGF_prob.and.copymap_prob) exit
	      if (original(i, j, k)%map /= testmap) copymap_prob = .true.
	      do l = 1, arraysize
	         if (copyGF_prob.and.copymap_prob) exit
		 if (real(original(i, j, k)%GF(l)) /= testGF) &
		     copyGF_prob = .true.
		 if (aimag(original(i, j, k)%GF(l)) /= testGF) &
		     copyGF_prob = .true.
	      enddo
	   enddo
	enddo
     enddo

     assert_false(copyGF_prob)
     assert_false(copymap_prob)
     deallocate(original, copy)
end test

test copy_slices_to_from
     complex(real12), allocatable :: original(:, :, :, :)
     complex(real12), allocatable  :: slice(:, :, :)
     type(greensfunc), allocatable :: copy(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     integer, parameter            :: insert_slice=2
     real(real12), parameter       :: testGF(2) = (/ one, two /)
     real(real12), parameter       :: insert = zero
     integer                       :: i, j, k, l, ierr
     logical                       :: copy_from_complex_prob = .false.
     logical                       :: copy_to_complex_prob = .false.
   

     allocate(copy(arraysize, arraysize, arraysize))
     call allocate_GF(copy, arraysize)

     allocate(original(arraysize, arraysize, arraysize, arraysize))
     allocate(slice(arraysize, arraysize, arraysize))

     do i = 1, arraysize
     	original(1:arraysize, 1:arraysize, 1:arraysize, i) = &
			      & cmplx(testGF(i), testGF(i))
     enddo

     do i = 1, arraysize
     	call copy_gf_slice(copy,&
	     & original(1:arraysize, 1:arraysize, 1:arraysize, i), i)
     enddo

     do i = 1, arraysize
     	if (copy_from_complex_prob) exit
	do j = 1, arraysize
	   if (copy_from_complex_prob) exit
	   do k = 1, arraysize
	       if (copy_from_complex_prob) exit
	       do l =1, arraysize
		  if (real(copy(i, j, k)%GF(l)).ne.testGF(l)) then
		     copy_from_complex_prob = .true.
		     exit
		  endif
		  if (aimag(copy(i, j, k)%GF(l)).ne.testGF(l)) then
		     copy_from_complex_prob = .true.
	    	     exit
		  endif
               enddo
	   enddo
	enddo      
     enddo

     assert_false(copy_from_complex_prob)

     call copy_gf_slice(slice,copy,insert_slice)

     do i = 1, arraysize
     	if (copy_to_complex_prob) exit
	do j = 1, arraysize
	   if (copy_to_complex_prob) exit
	   do k = 1, arraysize
	       if (copy_to_complex_prob) exit
	       if (real(slice(i, j, k)).ne.testGF(insert_slice)) then
		  copy_to_complex_prob = .true.
		  exit
	       endif
	       if (aimag(slice(i, j, k)).ne.testGF(insert_slice)) then
		  copy_to_complex_prob = .true.
	    	  exit
	       endif
	   enddo
	enddo      
     enddo    

     assert_false(copy_to_complex_prob)
     
 end test
end test_suite
