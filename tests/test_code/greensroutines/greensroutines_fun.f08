! greensroutines_fun.f90 - a unit test suite for greensroutines.f90
!
! funit generated this file from greensroutines.fun

module greensroutines_fun

 use constants
 use definedtypes
 use greensroutines

 implicit none

 logical :: noAssertFailed

 public :: test_greensroutines

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



 contains



 subroutine allocateGF_sizes

     integer, parameter            :: array_size = 2
     type(greensfunc), allocatable :: testGF(:, :, :)
     logical                       :: GFsize_prob = .false.
     logical                       :: get_size_prob = .false.
     integer                       :: i, j, k, itest, igettest

     allocate(testGF(array_size, array_size, array_size))
     call allocate_GF(testGF, array_size)

  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(size(testGF, 1)== array_size)) then
      print *, " *Assert_Equal failed* in test allocateGF_sizes &
              &[greensroutines.fun:21]"
      print *, "  ", "size(testGF, 1) (",size(testGF, 1),") is not",  array_size
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(size(testGF, 2)== array_size)) then
      print *, " *Assert_Equal failed* in test allocateGF_sizes &
              &[greensroutines.fun:22]"
      print *, "  ", "size(testGF, 2) (",size(testGF, 2),") is not",  array_size
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(size(testGF, 3)== array_size)) then
      print *, " *Assert_Equal failed* in test allocateGF_sizes &
              &[greensroutines.fun:23]"
      print *, "  ", "size(testGF, 3) (",size(testGF, 3),") is not",  array_size
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (GFsize_prob) then
      print *, " *Assert_False failed* in test allocateGF_sizes &
              &[greensroutines.fun:41]"
      print *, "  ", "GFsize_prob is not false"
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
    if (get_size_prob) then
      print *, " *Assert_False failed* in test allocateGF_sizes &
              &[greensroutines.fun:42]"
      print *, "  ", "get_size_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
      deallocate(testGF)

  numTests = numTests + 1

 end subroutine allocateGF_sizes



 subroutine calculateGF_output_withhyb

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test calculateGF_output_withhyb &
              &[greensroutines.fun:75]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (outwh_prob) then
      print *, " *Assert_False failed* in test calculateGF_output_withhyb &
              &[greensroutines.fun:107]"
      print *, "  ", "outwh_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine calculateGF_output_withhyb


 subroutine calculateGF_output_withouthyb

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr==0)) then
      print *, " *Assert_Equal failed* in test calculateGF_output_withouthyb &
              &[greensroutines.fun:135]"
      print *, "  ", "ierr (",ierr,") is not", 0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (outnoh_prob) then
      print *, " *Assert_False failed* in test calculateGF_output_withouthyb &
              &[greensroutines.fun:161]"
      print *, "  ", "outnoh_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine calculateGF_output_withouthyb


 subroutine calculateGF_divbyzero

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test calculateGF_divbyzero &
              &[greensroutines.fun:197]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine calculateGF_divbyzero


 subroutine GF_copying_individual_components

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (copyGF_prob) then
      print *, " *Assert_False failed* in test GF_copying_individual_components &
              &[greensroutines.fun:250]"
      print *, "  ", "copyGF_prob is not false"
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
    if (copymap_prob) then
      print *, " *Assert_False failed* in test GF_copying_individual_components &
              &[greensroutines.fun:251]"
      print *, "  ", "copymap_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(original, copy)

  numTests = numTests + 1

 end subroutine GF_copying_individual_components


 subroutine GF_invert

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
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(all(ierr1 ==  0))) then
      print *, " *Assert_True failed* in test GF_invert &
              &[greensroutines.fun:282]"
      print *, "  ", "all(ierr1 ==  0) is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (invertGF_prob) then
      print *, " *Assert_False failed* in test GF_invert &
              &[greensroutines.fun:301]"
      print *, "  ", "invertGF_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(test, ierr1)

  numTests = numTests + 1

 end subroutine GF_invert


 subroutine GFinvert_ierr_divby0

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
  ! Assert_True assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(any(ierr_invertGF == 1))) then
      print *, " *Assert_True failed* in test GFinvert_ierr_divby0 &
              &[greensroutines.fun:333]"
      print *, "  ", "any(ierr_invertGF == 1) is not true"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     deallocate(test)

  numTests = numTests + 1

 end subroutine GFinvert_ierr_divby0


 subroutine GF_reduction

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_reduction &
              &[greensroutines.fun:381]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (reduce_problem) then
      print *, " *Assert_False failed* in test GF_reduction &
              &[greensroutines.fun:404]"
      print *, "  ", "reduce_problem is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif


  numTests = numTests + 1

 end subroutine GF_reduction


 subroutine reduceGF_ierr1

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr1 &
              &[greensroutines.fun:427]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call reduceGF(test, fine, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr1 &
              &[greensroutines.fun:430]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine reduceGF_ierr1


 subroutine reduceGF_ierr2

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr2 &
              &[greensroutines.fun:450]"
      print *, "  ", "ierr (",ierr,") is not",  2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine reduceGF_ierr2


 subroutine reduceGF_ierr3

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 3)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr3 &
              &[greensroutines.fun:469]"
      print *, "  ", "ierr (",ierr,") is not",  3
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine reduceGF_ierr3


 subroutine GF_overloaded_equals

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (copyGF_prob) then
      print *, " *Assert_False failed* in test GF_overloaded_equals &
              &[greensroutines.fun:520]"
      print *, "  ", "copyGF_prob is not false"
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
    if (copymap_prob) then
      print *, " *Assert_False failed* in test GF_overloaded_equals &
              &[greensroutines.fun:521]"
      print *, "  ", "copymap_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(original, copy)

  numTests = numTests + 1

 end subroutine GF_overloaded_equals


 subroutine copy_slices_to_from

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (copy_from_complex_prob) then
      print *, " *Assert_False failed* in test copy_slices_to_from &
              &[greensroutines.fun:575]"
      print *, "  ", "copy_from_complex_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (copy_to_complex_prob) then
      print *, " *Assert_False failed* in test copy_slices_to_from &
              &[greensroutines.fun:597]"
      print *, "  ", "copy_to_complex_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

  numTests = numTests + 1

 end subroutine copy_slices_to_from


 subroutine funit_setup

  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown

 end subroutine funit_teardown


 subroutine test_greensroutines( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call allocateGF_sizes
  call funit_teardown

  call funit_setup
  call calculateGF_output_withhyb
  call funit_teardown

  call funit_setup
  call calculateGF_output_withouthyb
  call funit_teardown

  call funit_setup
  call calculateGF_divbyzero
  call funit_teardown

  call funit_setup
  call GF_copying_individual_components
  call funit_teardown

  call funit_setup
  call GF_invert
  call funit_teardown

  call funit_setup
  call GFinvert_ierr_divby0
  call funit_teardown

  call funit_setup
  call GF_reduction
  call funit_teardown

  call funit_setup
  call reduceGF_ierr1
  call funit_teardown

  call funit_setup
  call reduceGF_ierr2
  call funit_teardown

  call funit_setup
  call reduceGF_ierr3
  call funit_teardown

  call funit_setup
  call GF_overloaded_equals
  call funit_teardown

  call funit_setup
  call copy_slices_to_from
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_greensroutines

end module greensroutines_fun
