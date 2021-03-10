module greensroutines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Contains type definitions and functions associated with manipulating
!# the greensfunc type
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use constants, only: real12, one, cmplx_zero, tolerance
  use definedtypes, only: kappagrid
  implicit none
  private
  public allocate_GF, calculateGF, greensfunc, copymap, copyGF, invertGF,&
    reduceGF, assignment (=), copy_gf_slice, initialise_GF, allocate_3DGF
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type, public :: greensfunc
     !# Green's function type
     complex(real12),allocatable :: GF(:)
     !# value of Green's function wrt omega
     integer                     :: map = -1
     !# labels the associated coarse grid point (-1 means unassigned)
     integer, private            :: nGF_points = 0
     !# size the size of GF (0 means not yet allocated)
   contains
     procedure, public  :: get_size
     !# fetches the number of nGF_points
     !# by design, nGF_points should be identical for all members of same array
  end type greensfunc
  interface assignment (=)
     module procedure copy_all_gf
  end interface assignment (=)
  interface copy_gf_slice
     module procedure copy_slice_to_complex
     module procedure copy_slice_from_complex
  end interface copy_gf_slice
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
contains
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine allocate_GF(GF_array, n_points)
    type(greensfunc), intent(inout) :: GF_array
    integer, intent(in)             :: n_points

    GF_array%nGF_points = n_points
    allocate(GF_array%GF(n_points))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine allocate_GF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function get_size(this)
    integer           :: get_size
    class(greensfunc) :: this
    
    get_size = this%nGF_points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end function get_size
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine initialise_GF(GF_array, GF_init)
    type(greensfunc), intent(inout) :: GF_array
    complex(real12), intent(in)     :: GF_init

    integer :: nGF_points, i

    nGF_points = size(GF_array%GF, 1)
    do i = 1, nGF_points
       GF_array%GF(i)=GF_init
    enddo
  end subroutine initialise_GF
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine calculateGF(GFval, deltaw, dispersion, hybridisation,&
       ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Calculate the value of the greens function with or without hybridisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- Routine completed successfully;
!#              ierr = 1 -- Singular value of GF for at least one point.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout)              :: GFval(:, :, :)
    !# greensfuction output
    complex(real12), intent(in)                  :: deltaw
    !# frequency spacing
    type(kappagrid), intent(in)                  :: dispersion(:, :, :)
    !# dispersion in k-space
    type(greensfunc), intent(in), optional       :: hybridisation(:, :, :)
    !# hybridisation function
    integer, intent(out)                         :: ierr
    !# error code
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Maps fineGF grid values to coarse grid values (i.e. sum over k~)
!# Assigns coarseGF labels to coarseGF%map     
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- Routine completed successfully;
!#              ierr = 1 -- GF component of type(greensfunc) vars not allocated;
!#              ierr = 2 -- GF component of input differs from that of output;
!#              ierr = 3 -- coarseGF has fewer k components than fineGF.    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    type(greensfunc), intent(inout) :: coarseGF(:, :, :)
    !# coarse grid greensfuction (output)
    type(greensfunc), intent(in)    :: fineGF(:, :, :)
    !# fine grid greensfunction (input)
    integer, intent(out)            :: ierr
    !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer                       :: ncx, ncy, ncz
    integer                       :: nfx, nfy, nfz
    integer                       :: i, j, k, site_count
    integer                       :: nomega, nomega1, ier1
    type(greensfunc), allocatable :: workGFF(:, :, :)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! first error check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ((.not.allocated(fineGF(1, 1, 1)%GF))&
         .or.(.not.allocated(coarseGF(1, 1, 1)%GF))) then
       ierr = 1
       return
    else
       ierr = 0
    end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! fetch array bounds
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    ncx = size(coarseGF, 1)
    ncy = size(coarseGF, 2)
    ncz = size(coarseGF, 3)

    nfx = size(fineGF, 1)
    nfy = size(fineGF, 2)
    nfz = size(fineGF, 3)

    nomega = size(fineGF(1, 1, 1)%GF, 1)
    nomega1 = size(coarseGF(1, 1, 1)%GF, 1)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! second set of error checks
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (nomega.ne.nomega1) then
       ierr = 2
       return
    elseif ((nfx.le.ncx).or.(nfy.le.ncy).or.(nfz.le.ncz)) then
       ierr = 3
       return
    else
       continue
    end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! main routine
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    allocate(workGFF(nfx, nfy, nfz))
    call allocate_GF(workGFF, nomega)
    
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  contains
!------------------------------------------------------------------------------ 
    function sumfineGF(work, isite)
      type(greensfunc) :: sumfineGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Sums values of work%GF where work%map equals isite
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      type(greensfunc), intent(in) :: work (:, :, :)
      integer, intent(in):: isite
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      integer :: fx, fy, fz, fl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      sumfineGF%map= isite

      allocate(sumfineGF%GF(nomega))
      
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end function sumfineGF
!------------------------------------------------------------------------------ 
  end subroutine reduceGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine invertGF(GF, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Inverts greensfunction%GF component
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes : ierr = 0 -- no problems;
!#               ierr = 0 -- division by zero.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    type(greensfunc), intent(inout) :: GF
    !# Greensfunction to be inverted (output in same array)
    integer, intent(out)            :: ierr
    !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! error check
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    ierr = 0
    check_div_by_zero:if ((any(abs(real(GF%GF)).lt.tolerance))&
         .and.(any(abs(aimag(GF%GF)).lt.tolerance))) then
       ierr = 1
       return
    end if check_div_by_zero
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    GF%GF = one/GF%GF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine invertGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine copymap(copy, original)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!#  Copies original%map to copy%map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout) :: copy
    !# output array
    type(greensfunc), intent(in)    :: original
    !# input array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    copy%map = original%map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine copymap
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine copyGF(copy, original)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Copies original%GF to copy%GF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout) :: copy
    !# output array
    type(greensfunc), intent(in)    :: original
    !# input array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    copy%GF = original%GF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end subroutine copyGF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine copy_all_gf(copy, original)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Copies all of original to copy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout) :: copy
    !# output array
    type(greensfunc), intent(in)    :: original
    !# input array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    copy%GF = original%GF
    copy%map = original%map
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end subroutine copy_all_gf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine copy_slice_to_complex(copy, original, slice_no)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Copies slice of GF component of type(greensfunc) to complex array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    complex(real12), intent(inout)  :: copy
    !# output array
    type(greensfunc), intent(in)    :: original
    !# input array
    integer, intent(in)             :: slice_no
    !# green's function slice to be copied
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    copy = original%GF(slice_no) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end subroutine copy_slice_to_complex
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  elemental subroutine copy_slice_from_complex(copy, original, slice_no)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Copies complex array to slice of GF compenent of type(greensfunc)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout)   :: copy
    !# output array
    complex(real12), intent(in) :: original
    !# input array
    integer, intent(in)             :: slice_no
    !# green's function slice to be copied
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    copy%GF(slice_no) = original
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end subroutine copy_slice_from_complex
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine allocate_3DGF(Gfunc, nx, ny, nz, nomega, ierror)
    type(greensfunc), allocatable, intent(inout) :: Gfunc(:, :, :)
    integer, intent(in) :: nx, ny, nz, nomega
    integer, intent(out) :: ierror

    ierror = 0
    ! test inputs
    if ((nx.le.0).or.(ny.le.0).or.(nz.le.0).or.(nomega.le.0)) ierror = 1
    if (allocated(Gfunc)) ierror = 2
    if (ierror.ne.0) return

    allocate(Gfunc(nx, ny, nz))
    if (.not.allocated(Gfunc)) then
      ierror = 3
      return
    end if  
    call allocate_GF(Gfunc, nomega)
  
    if (any(test_not_allocated(Gfunc))) then
      ierror = 4
      return
    end if
        
  contains

    subroutine check_input(ierr)
       integer, intent(inout) :: ierr

       if ((nx.le.0).or.(ny.le.0).or.(nz.le.0).or.(nomega.le.0)) ierr = 1
       if (allocated(Gfunc)) ierr = 2
    end subroutine check_input

    elemental function test_not_allocated(Gtest)
      logical :: test_not_allocated
      type(greensfunc), intent(in) :: Gtest
      
      test_not_allocated = .not.allocated(Gtest%GF)
    end function

   end subroutine allocate_3DGF
end module greensroutines
