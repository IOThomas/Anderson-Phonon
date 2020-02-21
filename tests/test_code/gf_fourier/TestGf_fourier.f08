
! TestRunner.f90 - runs fUnit test suites
!
! funit generated this file on 2020-02-21 16:31:17 +0000.

program TestRunner

    use gf_fourier_fun
  
  implicit none

  integer, dimension(1) :: numTests, numAsserts, numAssertsTested, numFailures

    write(*,*)
  write(*,*) "gf_fourier test suite:"
  call test_gf_fourier &
    ( numTests(1), numAsserts(1), numAssertsTested(1), numFailures(1) )
  write(*,1) numAssertsTested(1), numAsserts(1), &
    numTests(1)-numFailures(1), numTests(1)
  1 format('Passed ',i0,' of ',i0,' possible asserts comprising ',i0,' of ',i0,' tests.')
  
  write(*,*)
  write(*,'(a)') "==========[ SUMMARY ]=========="
      write(*,'(a12)',advance="no") " gf_fourier:"
  if ( numFailures(1) == 0 ) then
    write(*,*) " passed"
  else
    write(*,*) " failed   <<<<<"
  end if
    write(*,*)

  if ( sum(numFailures) /= 0 ) stop 1

end program TestRunner
