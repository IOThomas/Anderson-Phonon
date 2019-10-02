! dispersions_fun.f90 - a unit test suite for dispersions.f90
!
! funit generated this file from dispersions.fun

module dispersions_fun

 use constants
 use dispersions

 implicit none

 logical :: noAssertFailed

 public :: test_dispersions

 private

 integer :: numTests          = 0
 integer :: numAsserts        = 0
 integer :: numAssertsTested  = 0
 integer :: numFailures       = 0



!Global variables

 contains



 subroutine fine_disp_zero

     real(real12) :: test
     real(real12) :: zero = 0.0_real12
     real(real12) :: tolerance = epsilon(test)

     test=fineDispersion(zero, zero, zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (test) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (test) )) then
      print *, " *Assert_Equal_Within failed* in test fine_disp_zero &
              &[dispersions.fun:19]"
      print *, "  ", "test (",test,") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_disp_zero


 subroutine funit_setup

  noAssertFailed = .true.
 end subroutine funit_setup


 subroutine funit_teardown

 end subroutine funit_teardown


 subroutine test_dispersions( nTests, nAsserts, nAssertsTested, nFailures )

  integer :: nTests
  integer :: nAsserts
  integer :: nAssertsTested
  integer :: nFailures

  continue

  call funit_setup
  call fine_disp_zero
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_dispersions

end module dispersions_fun
