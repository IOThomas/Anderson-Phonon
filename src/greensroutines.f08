module greensroutines

  use constants, only: real12, one, cmplx_zero, tolerance
  use definedtypes, only: kappagrid
  implicit none
  private
  public allocateGF,  calculateGF, greensfunc, copymap, copyGF, invertGF,&
       reduceGF
       

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type, public :: greensfunc
     complex(real12),allocatable :: GF(:)    !value of GF for omega2
     integer                     :: map   !coarse grid label assoc with kpoint
  end type greensfunc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine allocateGF(GFvariable, xsize, ysize, zsize, nomega, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Allocates arrays for greensfunc type variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Error codes: ierr = 0 -- Routine executed successfully
!              ierr = 1 -- Array size less than 1 in at least one dimension
!              ierr = 2 -- Array already allocated
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    type(greensfunc), allocatable, intent(inout) :: GFvariable(:, :, :)
    integer, intent(in)                          :: xsize
    integer, intent(in)                          :: ysize
    integer, intent(in)                          :: zsize
    integer, intent(in)                          :: nomega
    integer, intent(out)                         :: ierr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer :: i, j, k
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! check for errors
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ((xsize.lt.1).or.(ysize.lt.1).or.(zsize.lt.1).or.(nomega.lt.1)) then
       ierr = 1
       return
    elseif (allocated(GFvariable)) then
       ierr = 2
       return
    else
       ierr = 0
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    allocate(GFvariable(xsize, ysize, zsize))
    do i = 1, xsize
       do j = 1, ysize
          do k = 1, zsize
             allocate(GFvariable(i,j,k)%GF(nomega))
          enddo
       end do
    end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine allocateGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine calculateGF(GFval, deltaw, dispersion, hybridisation,&
       ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calculate the value of the greens function with or without hybridisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Error codes: ierr = 0 -- Routine completed successfully
!              ierr = 1 -- Singular value of GF for at least one point
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout)              :: GFval(:, :, :)
    complex(real12), intent(in)                  :: deltaw
    type(kappagrid), intent(in)                  :: dispersion(:, :, :)
    type(greensfunc), intent(in), optional       :: hybridisation(:, :, :)
    integer, intent(out)                         :: ierr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer :: i, j, k, l
    integer :: xsize, ysize, zsize, nomega
    complex(real12) :: work
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ierr = 0

    xsize = size(dispersion, 1)
    ysize = size(dispersion, 2)
    zsize = size(dispersion, 3)
    nomega = size(GFval(1,1,1)%GF, 1) ! probably a safe value!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    loop_over_all_points:do i = 1, xsize
       do j = 1, ysize
          do k = 1, zsize
             do l = 1, nomega
                test_optional_variable:if (present(hybridisation)) then
                   work = -hybridisation(i, j, k)%GF(l)
                else
                   work = cmplx_zero
                endif test_optional_variable
             
                work = work + real(l**2, real12) * deltaw * deltaw&
                     - dispersion(i, j, k)%omega2
                
                check_div_by_zero:if ((abs(real(work)).lt.tolerance)&
                     .and.(abs(aimag(work)).lt.tolerance)) then
                   ierr = 1
                   return
                end if check_div_by_zero
                
                GFval(i, j, k)%GF(l) = one/work
             enddo
          enddo
       end do
    end do loop_over_all_points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine calculateGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine reduceGF(coarseGF, fineGF, ierr)
    type(greensfunc), intent(inout) :: coarseGF(:, :, :)
    type(greensfunc), intent(in)    :: fineGF(:, :, :)
    integer, intent(out) :: ierr

    integer :: ncx, ncy, ncz
    integer :: nfx, nfy, nfz
    integer :: i, j, k, l, site_count
    integer :: nomega, ier1
    complex(real12) :: work
    type(greensfunc), allocatable :: workGFF(:, :, :)

    ncx = size(coarseGF, 1)
    ncy = size(coarseGF, 2)
    ncz = size(coarseGF, 3)

    nfx = size(fineGF, 1)
    nfy = size(fineGF, 2)
    nfz = size(fineGF, 3)

    nomega = size(fineGF(1,1,1)%GF, 1)

    call allocateGF(workGFF, nfx, nfy, nfz, nomega, ier1)
    ! error handling code here
    call copymap(workGFF, fineGF)
    call copyGF(workGFF, fineGF)
    
    do i = 1, ncx
       do j = 1, ncy
          do k = 1, ncz
             site_count = i + (j - 1)*ncx + (k - 1)*ncx*ncy
             coarseGF(i, j, k) = sumfineGF(workGFF, site_count)
          enddo
       enddo
    enddo

  contains

    function sumfineGF(work, isite)
      type(greensfunc) :: sumfineGF
      type(greensfunc) :: work (:, :, :)
      integer, intent(in):: isite
      
      integer :: fx, fy, fz, fl, imap

      sumfineGF%map= isite

      do fl = 1, nomega
         sumfineGF%GF(fl) = cmplx_zero
         do fx = 1, nfx
            do fy = 1, nfy
               do fz = 1, nfz
                  if (sumfineGF%map == work(fx, fy, fz)%map) then
                     sumfineGF%GF(fl) = sumfineGF%GF(fl) &
                          + work(fx, fy, fz)%GF(fl)
                  endif
               enddo
            enddo
         enddo
      end do
    end function sumfineGF
      

  end subroutine reduceGF

  elemental subroutine invertGF(GF, ierr)
    type(greensfunc), intent(inout) :: GF
    integer, intent(out)                :: ierr

    check_div_by_zero:if ((any(abs(real(GF%GF)).lt.tolerance))&
         .and.(any(abs(aimag(GF%GF)).lt.tolerance))) then
       ierr = 1
       return
    end if check_div_by_zero

    GF%GF = one/GF%GF
  end subroutine invertGF

  
  elemental subroutine copymap(copy, original)
    type(greensfunc), intent(inout) :: copy
    type(greensfunc), intent(in)  :: original

    copy%map = original%map
  end subroutine copymap

  elemental subroutine copyGF(copy, original)
    type(greensfunc), intent(inout) :: copy
    type(greensfunc), intent(in)  :: original

    copy%GF = original%GF
  end subroutine copyGF
    
end module greensroutines
