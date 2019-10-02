
! TestRunner.f90 - runs fUnit test suites
!
! funit generated this file on 2019-10-02 18:56:53 +0100.

program TestRunner

    use dispersions_fun
  
  implicit none

  integer, dimension(1) :: numTests, numAsserts, numAssertsTested, numFailures

    write(*,*)
  write(*,*) "dispersions test suite:"
  call test_dispersions &
    ( numTests(1), numAsserts(1), numAssertsTested(1), numFailures(1) )
  write(*,1) numAssertsTested(1), numAsserts(1), &
    numTests(1)-numFailures(1), numTests(1)
  1 format('Passed ',i0,' of ',i0,' possible asserts comprising ',i0,' of ',i0,' tests.')
  
  write(*,*)
  write(*,'(a)') "==========[ SUMMARY ]=========="
      write(*,'(a13)',advance="no") " dispersions:"
  if ( numFailures(1) == 0 ) then
    write(*,*) " passed"
  else
    write(*,*) " failed   <<<<<"
  end if
    write(*,*)

  if ( sum(numFailures) /= 0 ) stop 1

end program TestRunner
