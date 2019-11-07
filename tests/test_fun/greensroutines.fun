test_suite greensroutines

setup

end setup

teardown

end teardown

test allocateGF_sizes
     integer, parameter            :: array_size = 2
     type(greensfunc), allocatable :: testGF(:, :, :)
     integer                       :: ierr
     logical                       :: GFsize_prob = .false.
     integer                       :: i, j, k, itest

     call allocateGF(testGF, array_size, array_size, array_size, array_size,&
     	  ierr)

     assert_equal(size(testGF, 1), array_size)
     assert_equal(size(testGF, 2), array_size)
     assert_equal(size(testGF, 3), array_size)
     assert_equal(ierr, 0)

     do i = 1, array_size
     	if (GFsize_prob) then
	   exit
	endif
     	do j = 1, array_size
	   if (GFsize_prob) then
	      exit
	   endif
	   do k = 1, array_size
	      itest = size(testGF(i, j, k)%GF, 1)
	      if (itest.ne.array_size) then
	      	 GFsize_prob = .true.
		 exit
	      end if
	   enddo
	enddo
      enddo

      assert_false(GFsize_prob)
      deallocate(testGF)
end test

test allocateGF_ierr_code1
     integer, parameter            :: array_size = 2
     integer, parameter            :: array_wrong = -10
     type(greensfunc), allocatable :: testGF(:, :, :)
     integer                       :: ierr

     call allocateGF(testGF, array_wrong, array_size, array_size, array_size,&
     	  ierr)
     assert_equal(ierr, 1)
     

     call allocateGF(testGF, array_size, array_wrong, array_size, array_size,&
     	  ierr)
     assert_equal(ierr, 1)
     

     call allocateGF(testGF, array_size, array_size, array_wrong, array_size,&
     	  ierr)
     assert_equal(ierr, 1)
    

     call allocateGF(testGF, array_wrong, array_size, array_size, array_wrong,&
     	  ierr)
     assert_equal(ierr, 1)
     
end test

test allocateGF_ierr_code2
     integer, parameter            :: array_size = 2
     type(greensfunc), allocatable :: testGF(:, :, :)
     integer                       :: ierr

     allocate(testGF(array_size,array_size,array_size))
     call allocateGF(testGF, array_size, array_size, array_size, array_size,&
     	  ierr)
     assert_equal(ierr, 2)
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

end test_suite
