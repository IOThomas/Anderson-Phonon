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
     integer                       :: ierr
     logical                       :: GFsize_prob = .false.
     integer                       :: i, j, k, itest

     call allocateGF(testGF, array_size, array_size, array_size, array_size,&
     	  ierr)

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
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test allocateGF_sizes &
              &[greensroutines.fun:24]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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

  ! Assert_False assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (GFsize_prob) then
      print *, " *Assert_False failed* in test allocateGF_sizes &
              &[greensroutines.fun:44]"
      print *, "  ", "GFsize_prob is not false"
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


 subroutine allocateGF_ierr_code1

     integer, parameter            :: array_size = 2
     integer, parameter            :: array_wrong = -10
     type(greensfunc), allocatable :: testGF(:, :, :)
     integer                       :: ierr

     call allocateGF(testGF, array_wrong, array_size, array_size, array_size,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test allocateGF_ierr_code1 &
              &[greensroutines.fun:56]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

     call allocateGF(testGF, array_size, array_wrong, array_size, array_size,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test allocateGF_ierr_code1 &
              &[greensroutines.fun:61]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

     call allocateGF(testGF, array_size, array_size, array_wrong, array_size,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test allocateGF_ierr_code1 &
              &[greensroutines.fun:66]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
    

     call allocateGF(testGF, array_wrong, array_size, array_size, array_wrong,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test allocateGF_ierr_code1 &
              &[greensroutines.fun:71]"
      print *, "  ", "ierr (",ierr,") is not",  1
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     

  numTests = numTests + 1

 end subroutine allocateGF_ierr_code1


 subroutine allocateGF_ierr_code2

     integer, parameter            :: array_size = 2
     type(greensfunc), allocatable :: testGF(:, :, :)
     integer                       :: ierr

     allocate(testGF(array_size,array_size,array_size))
     call allocateGF(testGF, array_size, array_size, array_size, array_size,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test allocateGF_ierr_code2 &
              &[greensroutines.fun:83]"
      print *, "  ", "ierr (",ierr,") is not",  2
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     deallocate(testGF)

  numTests = numTests + 1

 end subroutine allocateGF_ierr_code2


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
              &[greensroutines.fun:115]"
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
              &[greensroutines.fun:147]"
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
              &[greensroutines.fun:175]"
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
              &[greensroutines.fun:201]"
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
              &[greensroutines.fun:237]"
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

     call allocateGF(original, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_copying_individual_components &
              &[greensroutines.fun:253]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(copy, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_copying_individual_components &
              &[greensroutines.fun:257]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     
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
              &[greensroutines.fun:292]"
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
              &[greensroutines.fun:293]"
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

     call allocateGF(test, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_invert &
              &[greensroutines.fun:309]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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
              &[greensroutines.fun:325]"
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
              &[greensroutines.fun:344]"
      print *, "  ", "invertGF_prob is not false"
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
     
     deallocate(test,ierr1)

  numTests = numTests + 1

 end subroutine GF_invert


 subroutine GFinvert_ierr_divby0

     type(greensfunc), allocatable :: test(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     integer                       :: i, j, k, l, ierr
     integer, allocatable          :: ierr_invertGF(:, :, :)

     call allocateGF(test, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GFinvert_ierr_divby0 &
              &[greensroutines.fun:359]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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
              &[greensroutines.fun:377]"
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

     call allocateGF(coarse, coarse_size, coarse_size, coarse_size, nomega, &
         ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_reduction &
              &[greensroutines.fun:396]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(fine, fine_size, fine_size, fine_size, nomega, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_reduction &
              &[greensroutines.fun:399]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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
              &[greensroutines.fun:426]"
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
              &[greensroutines.fun:449]"
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

     call allocateGF(coarse, coarse_size, coarse_size, coarse_size, nomega, &
         ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr1 &
              &[greensroutines.fun:464]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(fine, fine_size, fine_size, fine_size, nomega, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr1 &
              &[greensroutines.fun:467]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     allocate(test(1, 1, 1))
     
     call reduceGF(coarse, test, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 1)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr1 &
              &[greensroutines.fun:472]"
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
              &[greensroutines.fun:475]"
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

     call allocateGF(coarse, coarse_size, coarse_size, coarse_size, nomega, &
         ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr2 &
              &[greensroutines.fun:490]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(fine, fine_size, fine_size, fine_size, nomega1, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr2 &
              &[greensroutines.fun:493]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call reduceGF(coarse, fine, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 2)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr2 &
              &[greensroutines.fun:496]"
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

     call allocateGF(coarse, coarse_size, coarse_size, coarse_size, nomega, &
         ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr3 &
              &[greensroutines.fun:510]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(fine, fine_size, fine_size, fine_size, nomega, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr3 &
              &[greensroutines.fun:513]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call reduceGF(coarse, fine, ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 3)) then
      print *, " *Assert_Equal failed* in test reduceGF_ierr3 &
              &[greensroutines.fun:516]"
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


 subroutine GF_overloaded_allocation

     type(greensfunc), allocatable :: original(:, :, :)
     type(greensfunc), allocatable :: copy(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     real(real12), parameter       :: testGF = two
     integer                       :: i, j, k, l, ierr
     logical                       :: copymap_prob = .false.
     logical                       :: copyGF_prob = .false.

     call allocateGF(original, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_overloaded_allocation &
              &[greensroutines.fun:531]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     call allocateGF(copy, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test GF_overloaded_allocation &
              &[greensroutines.fun:535]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

     
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
      print *, " *Assert_False failed* in test GF_overloaded_allocation &
              &[greensroutines.fun:569]"
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
      print *, " *Assert_False failed* in test GF_overloaded_allocation &
              &[greensroutines.fun:570]"
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

 end subroutine GF_overloaded_allocation


 subroutine copy_slices_to_from

     complex(real12), allocatable :: original(:, :, :, :)
     complex(real12), allocatable :: slice(:, :, :)
     type(greensfunc), allocatable :: copy(:, :, :)
     integer, parameter            :: arraysize = 2
     integer, parameter            :: testmap = 2
     integer, parameter            :: insert_slice=2
     real(real12), parameter       :: testGF(2) = (/ one, two /)
     real(real12), parameter       :: insert = zero
     integer                       :: i, j, k, l, ierr
     logical                       :: copy_from_complex_prob = .false.
     logical                       :: copy_to_complex_prob = .false.
   

     call allocateGF(copy, arraysize, arraysize, arraysize, arraysize,&
     	  ierr)
  ! Assert_Equal assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.(ierr== 0)) then
      print *, " *Assert_Equal failed* in test copy_slices_to_from &
              &[greensroutines.fun:590]"
      print *, "  ", "ierr (",ierr,") is not",  0
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

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
              &[greensroutines.fun:625]"
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
              &[greensroutines.fun:647]"
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
  call allocateGF_ierr_code1
  call funit_teardown

  call funit_setup
  call allocateGF_ierr_code2
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
  call GF_overloaded_allocation
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
