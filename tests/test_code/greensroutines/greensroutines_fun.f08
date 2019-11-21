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


 subroutine GF_copying

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
      print *, " *Assert_Equal failed* in test GF_copying &
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
      print *, " *Assert_Equal failed* in test GF_copying &
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
      print *, " *Assert_False failed* in test GF_copying &
              &[greensroutines.fun:293]"
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
      print *, " *Assert_False failed* in test GF_copying &
              &[greensroutines.fun:294]"
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

 end subroutine GF_copying


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
              &[greensroutines.fun:310]"
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
              &[greensroutines.fun:326]"
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
              &[greensroutines.fun:345]"
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
              &[greensroutines.fun:360]"
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
              &[greensroutines.fun:378]"
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
  call GF_copying
  call funit_teardown

  call funit_setup
  call GF_invert
  call funit_teardown

  call funit_setup
  call GFinvert_ierr_divby0
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_greensroutines

end module greensroutines_fun
