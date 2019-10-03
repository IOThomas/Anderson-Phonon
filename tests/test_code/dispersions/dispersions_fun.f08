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



real(real12), parameter :: tolerance = epsilon(one) 

 contains



 subroutine fine_disp_zero

     complex(real12)         :: test    

     test = fineDispersion(zero, zero, zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_disp_zero &
              &[dispersions.fun:17]"
      print *, "  ", "real(test) (",real(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_disp_zero &
              &[dispersions.fun:18]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
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


 subroutine fine_max_kx

     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(input, zero, zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kx &
              &[dispersions.fun:27]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kx &
              &[dispersions.fun:28]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_max_kx


 subroutine fine_max_ky

     complex(real12)          :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(zero, input, zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_ky &
              &[dispersions.fun:37]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_ky &
              &[dispersions.fun:38]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_max_ky


 subroutine fine_max_kz

     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = one

     test = fineDispersion(zero, zero, input)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kz &
              &[dispersions.fun:47]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kz &
              &[dispersions.fun:48]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_max_kz


 subroutine fine_max_kall

     complex(real12)         :: test
     real(real12), parameter :: input = pi
     real(real12), parameter :: expected = three

     test = fineDispersion(input, input, input)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kall &
              &[dispersions.fun:57]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_max_kall &
              &[dispersions.fun:58]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_max_kall


 subroutine fine_periodicty_kx

     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(input1, zero, zero)
     test2 = fineDispersion(input2, zero, zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(test2) &
     +tolerance) &
     .ge. &
     (real(test1)) &
             .and. &
     (real(test2) &
     -tolerance) &
     .le. &
     (real(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kx &
              &[dispersions.fun:69]"
      print *, "  ", "real(test1) (",real(test1),") is not", &
 real(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(test2) &
     +tolerance) &
     .ge. &
     (aimag(test1)) &
             .and. &
     (aimag(test2) &
     -tolerance) &
     .le. &
     (aimag(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kx &
              &[dispersions.fun:70]"
      print *, "  ", "aimag(test1) (",aimag(test1),") is not", &
 aimag(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_periodicty_kx


 subroutine fine_periodicty_ky

     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(zero, input1 , zero)
     test2 = fineDispersion(zero, input1 , zero)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(test2) &
     +tolerance) &
     .ge. &
     (real(test1)) &
             .and. &
     (real(test2) &
     -tolerance) &
     .le. &
     (real(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_ky &
              &[dispersions.fun:81]"
      print *, "  ", "real(test1) (",real(test1),") is not", &
 real(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(test2) &
     +tolerance) &
     .ge. &
     (aimag(test1)) &
             .and. &
     (aimag(test2) &
     -tolerance) &
     .le. &
     (aimag(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_ky &
              &[dispersions.fun:82]"
      print *, "  ", "aimag(test1) (",aimag(test1),") is not", &
 aimag(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_periodicty_ky


 subroutine fine_periodicty_kz

     complex(real12)            :: test1
     complex(real12)            :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(zero, zero, input1)
     test2 = fineDispersion(zero, zero, input2)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(test2) &
     +tolerance) &
     .ge. &
     (real(test1)) &
             .and. &
     (real(test2) &
     -tolerance) &
     .le. &
     (real(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kz &
              &[dispersions.fun:93]"
      print *, "  ", "real(test1) (",real(test1),") is not", &
 real(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(test2) &
     +tolerance) &
     .ge. &
     (aimag(test1)) &
             .and. &
     (aimag(test2) &
     -tolerance) &
     .le. &
     (aimag(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kz &
              &[dispersions.fun:94]"
      print *, "  ", "aimag(test1) (",aimag(test1),") is not", &
 aimag(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_periodicty_kz


 subroutine fine_periodicty_kall

     complex(real12)         :: test1
     complex(real12)         :: test2 
     real(real12), parameter :: input1= pi + 0.1_real12
     real(real12), parameter :: input2= -pi + 0.1_real12

     test1 = fineDispersion(input1, input1, input1)
     test2 = fineDispersion(input2, input2, input2)
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((real(test2) &
     +tolerance) &
     .ge. &
     (real(test1)) &
             .and. &
     (real(test2) &
     -tolerance) &
     .le. &
     (real(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kall &
              &[dispersions.fun:105]"
      print *, "  ", "real(test1) (",real(test1),") is not", &
 real(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((aimag(test2) &
     +tolerance) &
     .ge. &
     (aimag(test1)) &
             .and. &
     (aimag(test2) &
     -tolerance) &
     .le. &
     (aimag(test1)) )) then
      print *, " *Assert_Equal_Within failed* in test fine_periodicty_kall &
              &[dispersions.fun:106]"
      print *, "  ", "aimag(test1) (",aimag(test1),") is not", &
 aimag(test2),"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine fine_periodicty_kall


 subroutine coarse_summation_someones

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
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_someones &
              &[dispersions.fun:125]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((zero &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (zero &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_someones &
              &[dispersions.fun:126]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 zero,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine coarse_summation_someones


 subroutine coarse_summation_allzero

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
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_allzero &
              &[dispersions.fun:142]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_allzero &
              &[dispersions.fun:143]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine coarse_summation_allzero


 subroutine coarse_summation_allone

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
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (real(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (real(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_allone &
              &[dispersions.fun:160]"
      print *, "  ", "real(test) (",real(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif
  ! Assert_Equal_Within assertion
  numAsserts = numAsserts + 1
  if (noAssertFailed) then
    if (.not.((expected &
     +tolerance) &
     .ge. &
     (aimag(test)) &
             .and. &
     (expected &
     -tolerance) &
     .le. &
     (aimag(test)) )) then
      print *, " *Assert_Equal_Within failed* in test coarse_summation_allone &
              &[dispersions.fun:161]"
      print *, "  ", "aimag(test) (",aimag(test),") is not", &
 expected,"within",tolerance
      print *, ""
      noAssertFailed = .false.
      numFailures    = numFailures + 1
    else
      numAssertsTested = numAssertsTested + 1
    endif
  endif

  numTests = numTests + 1

 end subroutine coarse_summation_allone


     

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

  call funit_setup
  call fine_max_kx
  call funit_teardown

  call funit_setup
  call fine_max_ky
  call funit_teardown

  call funit_setup
  call fine_max_kz
  call funit_teardown

  call funit_setup
  call fine_max_kall
  call funit_teardown

  call funit_setup
  call fine_periodicty_kx
  call funit_teardown

  call funit_setup
  call fine_periodicty_ky
  call funit_teardown

  call funit_setup
  call fine_periodicty_kz
  call funit_teardown

  call funit_setup
  call fine_periodicty_kall
  call funit_teardown

  call funit_setup
  call coarse_summation_someones
  call funit_teardown

  call funit_setup
  call coarse_summation_allzero
  call funit_teardown

  call funit_setup
  call coarse_summation_allone
  call funit_teardown

  nTests          = numTests
  nAsserts        = numAsserts
  nAssertsTested  = numAssertsTested
  nFailures       = numFailures

 end subroutine test_dispersions

end module dispersions_fun
