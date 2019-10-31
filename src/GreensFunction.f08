module greensroutines

  use constants, only: real12, one, cmplx_zero
  use definedtypes, only: greensfunc
  private
  implicit none
  private
  public allocateGF,  calculateGF

contains

  elemental subroutine allocateGF(GFvariable, xsize, ysize, zsize, nomega, ierr)
    type(greensfunc), allocatable, intent(inout) :: GFvariable
    integer, intent(in)                          :: xsize
    integer, intent(in)                          :: ysize
    integer, intent(in)                          :: zsize
    integer, intent(in)                          :: nomega
    integer, intent(out)                         :: ierr

    integer :: i, j, k

    if ((xsize.lt.1).or.(ysize.lt.1).or.(zsize.lt.1).or.(nomega.lt.1)) then
       ierr = 1
       return
    elseif (allocated(GFvariable)) then
       ierr = 2
       return
    else
       ierr = 0
    endif

    allocate(GFvariable(xsize, ysize, zsize))
    do i = 1, xsize
       do j = 1, ysize
          do k = 1, zsize
             allocate(GFvariable(i,j,k)%GF(nomega))
          enddo
       end do
    end do

  end subroutine allocateGF

  elemental subroutine allocateGF(GFval, deltaw, dispersion, hybridisation,&
       ierr)
    type(greensfunc), intent(inout)              :: GFval(:, :, :)
    complex(real12), intent(in)                  :: deltaw
    type(kappagrid), intent(in)                  :: dispersion(:, :, :)
    type(greensfunc), intent(in), optional       :: hybridisation(:, :, :)
    integer, intent(out)                         :: ierr

    integer :: i, j, k
    integer :: xsize, ysize, zsize, nomega
    complex(real12) :: work

    ierr = 0

    xsize = size(dispersion, 1)
    ysize = size(dispersion, 2)
    zsize = size(dispersion, 3)
    nomega = size(GFval(1,1,1)%GF, 1) ! probably a safe value!
    
    do i = 1, xsize
       do j = 1, ysize
          do k = 1, zsize
             do l = 1, nomega
                test_optional_variable:if (present(hybridisation)) then
                   work = -hybridisation(i, j, k)%GF
                else
                   work = cmplx_zero
                endif test_optional_variable
             
                work = work + real(l**2, real12) * omega_diff * omega_diff&
                     - dispersion(i, j, k)%omega2
                
                check_div_by_zero:if ((abs(real(work)).lt.tolerance)&
                     .or.(abs(aimag(work).lt.tolerance))) then
                   ierr = 1
                   return
                end if check_div_by_zero
                
                GFval(i, j, k)%GF(l) = work
             enddo
          enddo
       end do
    end do

    deallocate(work)
  end subroutine allocateGF
