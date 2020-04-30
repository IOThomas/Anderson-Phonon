module gf_fourier
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# This contains wrapper and related functions for FFTW routines.
!# Currently configured to use quadruple precision ('fftw3q_').
!#
!# To use: (1) call greensfunc_initplan for appropriate array size;
!#         (2) call gf_fft for those array sizes until no longer needed;
!#         (3) call greensfunc_killplan once done.  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use constants, only: real12, zero, cmplx_zero
  use greensroutines, only: greensfunc
  use, intrinsic :: iso_c_binding
  implicit none
  include 'fftw3.f03'
  include 'fftw3q.f03'
  private
  public greensfunc_initplan, greensfunc_killplan, get_gf_plan_status, &
       gf_fft, forward_fft, backward_fft
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! useful public constants defining FFT direction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer, parameter :: forward_fft = 1
  !# use as type_flag to call a forward fft
  integer, parameter :: backward_fft = -1
  !# use as type_flag to call a backward fft
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! private variables and pointers for interoprability with C FFT routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type(C_PTR) :: gf_plan_forward
  !stores information for forward GF FFT plan
  type(C_PTR) :: gf_plan_backward
  !stores information for backward GF FFT plan
  complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_in(:,:,:)
  complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_out(:,:,:)
  logical :: init_gfplan = .false.
  ! have the gf plans been initialised?

contains
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine greensfunc_initplan(greens_function, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Sets up FFTW parameters for computations.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- Routine executed successfully;
!#              ierr = 1 -- FFT parameters already set;
!#              ierr = 2-4 -- Null pointers for forward, backward or both FFTs
!#                       - a big problem!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(in) :: greens_function(:, :, :) 
    integer, intent(out)         :: ierr  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer :: x_size, y_size, z_size

    ierr = 0
    call check_input()
    if (ierr.ne.0) return

    x_size = size(greens_function, 1)
    y_size = size(greens_function, 2)
    z_size = size(greens_function, 3)

    allocate(gfwork_in(x_size, y_size, z_size))
    allocate(gfwork_out(x_size, y_size, z_size))

    ! note ctype array ordering
    gf_plan_forward = fftwq_plan_dft_3d(z_size, y_size, x_size, gfwork_in, &
         gfwork_out, FFTW_FORWARD, FFTW_ESTIMATE)

    gf_plan_backward = fftwq_plan_dft_3d(z_size, y_size, x_size, gfwork_in, &
         gfwork_out, FFTW_BACKWARD, FFTW_ESTIMATE)

    call check_plans()
    if (ierr.ne.0) return
    init_gfplan = .true.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  contains
!------------------------------------------------------------------------------
    subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
      if (init_gfplan) ierr = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end subroutine check_input
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
    subroutine check_plans()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (c_associated(gf_plan_forward, C_NULL_PTR)) then
         ierr = 2
      else if (c_associated(gf_plan_backward, C_NULL_PTR)) then
         ierr = 3
      else if ((c_associated(gf_plan_forward, C_NULL_PTR)).and.&
           (c_associated(gf_plan_backward, C_NULL_PTR))) then
         ierr = 4
      end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end subroutine check_plans
!------------------------------------------------------------------------------ 
  end subroutine greensfunc_initplan
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine greensfunc_killplan(ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Cleans up after all FFTs are done
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes : ierr = 0 -- no problems;
!#               ierr = 1 -- FFT settings haven't been initialised.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    integer, intent(out)   :: ierr
    !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ierr = 0
    call check_input()
    if (ierr.ne.0) return

    call fftwq_destroy_plan(gf_plan_forward)
    call fftwq_destroy_plan(gf_plan_backward)

    deallocate(gfwork_in, gfwork_out)

    init_gfplan = .false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  contains
!------------------------------------------------------------------------------ 
    subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (.not.(init_gfplan)) ierr = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end subroutine check_input
!------------------------------------------------------------------------------ 
  end subroutine greensfunc_killplan
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Check status of various private FFT related variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    logical, intent(out) :: init_plan
    !# is the FFT plan initialised?
    logical, intent(out) :: forward_associated
    !# is the forward FFT C pointer associated?
    logical, intent(out) :: backward_associated
    !# is the backward FFT C pointer associated?
    logical, intent(out) :: work_allocated
    !# are the work arrays allocated?
    logical, intent(out) :: forward_null
    !# is the forward FFT C pointer null?
    logical, intent(out) :: backward_null
    !# is the backward FFT C pointer null?
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    init_plan = init_gfplan
    work_allocated = (allocated(gfwork_in)).and.(allocated(gfwork_out))
    forward_associated = c_associated(gf_plan_forward)
    backward_associated = c_associated(gf_plan_backward)
    forward_null = c_associated(gf_plan_forward, C_NULL_PTR)
    backward_null = c_associated(gf_plan_backward, C_NULL_PTR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  end subroutine get_gf_plan_status
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine gf_fft(greens_function, type_flag, ierr)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# FFT of the matrix greens_function, output to greens_function
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes : ierr = 0 -- no problems;
!#               ierr = 1 -- FFT plan not initialised;
!#               ierr = 2 -- type_flag has invalid value;
!#               ierr = 3-5 -- *x*, *y* or *z* array sizes of greens_function
!#                        (respectively) don't match initialised settings.  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(greensfunc), intent(inout) :: greens_function(:, :, :)
    !# Green's function to be FFTed
    integer, intent(in) :: type_flag !must be -1 or +1
    !# set to **forward_fft** or **backward_fft** when calling subroutine
    integer, intent(out) :: ierr
    !# error code
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer, parameter :: forward_trans = 1, backward_trans = -1
    integer :: x_size, y_size, z_size, n_omega
    integer :: ix, iy, iz, iom
    real(real12) :: volume
    type(C_PTR) :: transform_type

    ierr = 0
    call check_input()
    if (ierr.ne.0) return

    x_size = size(greens_function, 1)
    y_size = size(greens_function, 2)
    z_size = size(greens_function, 3)
    volume = real(x_size*y_size*z_size, real12)
    n_omega = size(greens_function(1, 1, 1)%GF, 1)
    call check_work_array_bounds()
    if (ierr.ne.0) return

    if (type_flag.eq.backward_trans) then
       transform_type = gf_plan_backward
    else if (type_flag.eq.forward_trans) then
       transform_type = gf_plan_forward
    end if
    
    do iom = 1, n_omega
       do iz = 1, z_size
          do iy = 1, y_size
             do ix = 1, x_size
                gfwork_in(ix, iy, iz) = greens_function(ix, iy, iz)%GF(iom)
             enddo
          enddo
       enddo
       call fftwq_execute_dft(transform_type, gfwork_in, gfwork_out)
       do iz = 1, z_size
          do iy = 1, y_size
             do ix = 1, x_size
                !scale backwards transformation
                if (type_flag.eq.backward_trans) &
                     gfwork_out(ix, iy, iz) = gfwork_out(ix, iy, iz)/volume
                greens_function(ix, iy, iz)%GF(iom)=gfwork_out(ix, iy, iz)
             enddo
          enddo
       enddo
    enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  contains
!------------------------------------------------------------------------------
    subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (.not.(init_gfplan)) then
         ierr = 1
      else if ((type_flag.ne.backward_trans)&
           .and.(type_flag.ne.forward_trans)) then
         ierr = 2
      end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    end subroutine check_input
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------ 
    subroutine check_work_array_bounds()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if ((x_size.ne.size(gfwork_in, 1)).or.(x_size.ne.size(gfwork_out, 1))) &
           then
         ierr = 3
      else if ((y_size.ne.size(gfwork_in, 2)).or.&
           (y_size.ne.size(gfwork_out, 2))) then
         ierr = 4
      else if ((z_size.ne.size(gfwork_in, 3)).or.&
           (z_size.ne.size(gfwork_out, 3))) then
         ierr = 5
      endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    end subroutine check_work_array_bounds
!------------------------------------------------------------------------------
  end subroutine gf_fft
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine momspace_to_realspace(momentum, rlspace, ierr)
    type(greensfunc), intent(in) :: momentum(:, :, :)
    type(greensfunc), intent(out) :: rlspace(:, :)
    integer :: ierr

    !routine variables
    type(greensfunc), allocatable :: workreal(:, :, :, :, :, :)
    integer :: x_size, y_size, z_size, nomega
    integer :: npoints
    integer :: ierr1
    integer :: ix, iy, iz, jx, jy, jz, io

    x_size = size(momentum,1)
    y_size = size(momentum,2)
    z_size = size(momentum,3)
    nomega = size(momentum(1, 1, 1)%GF, 1)

    call error_check()

    call allocate_workreal(workreal, x_size, y_size, z_size,&
         & nomega, ierr1) ! allocates and sets all entries to zero
    if (ierr1.ne.0) call fatal_error_from_call(ierr1, "realspace_to_mo&
         &mspace", "allocate_workreal")
    
    copy_momspace_to_diagonal:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             do io = 1, nomega
                workreal(ix, iy, iz, ix, iy, iz)%GF(io) = &
                     & momentum(ix, iy, iz)%GF(io)
             enddo
          enddo
       enddo
    enddo copy_momspace_to_diagonal

    call fft_on_kdash(workreal, backward_fft)
    call fft_on_k(workreal, forward_fft)
 
    !reshape array
    rlspace = reshape(workreal, [npoints, npoints])

  contains

    subroutine error_check()

      if (x_size*y_size*z_size.ne.size(rlspace,1)) then
         ierr = 1
      elseif (x_size*y_size*z_size.ne.size(rlspace,2)) then
         ierr = 1
      elseif (size(rlspace,1).ne.size(rlspace,2)) then
         ierr = 2
      else
         ierr = 0
      endif

    end subroutine error_check

  end subroutine momspace_to_realspace

  subroutine realspace_to_momspace(rlspace, momentum, ierr)
    type(greensfunc), intent(out) :: momentum(:, :, :)
    type(greensfunc), intent(in) :: rlspace(:, :)
    integer :: ierr

    !routine variables
    type(greensfunc), allocatable :: workreal(:, :, :, :, :, :)
    integer :: x_size, y_size, z_size, nomega
    integer :: npoints
    integer :: ierr1
    integer :: ix, iy, iz, jx, jy, jz, io

    x_size = size(momentum,1)
    y_size = size(momentum,2)
    z_size = size(momentum,3)
    nomega = size(momentum(1, 1, 1)%GF, 1)
    call error_check()

    call allocate_workreal(workreal, x_size, y_size, z_size,&
         & nomega, ierr1) ! allocates and sets all entries to zero
    if (ierr1.ne.0) call fatal_error_from_call(ierr1, "momspace_to_rl&
         &space", "allocate_workreal")

    workreal = reshape(rlspace, [x_size, y_size, z_size, x_size,&
         & y_size, z_size])

    call fft_on_k(workreal,forward_fft)

    call fft_on_kdash(workreal,backward_fft)

    copy_diagonal_to_momspace:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             do io = 1, nomega
                momentum(ix, iy, iz)%GF(io) = &
                     & workreal(ix, iy, iz, ix, iy, iz)%GF(io)
             enddo
          enddo
       enddo
    enddo copy_diagonal_to_momspace

  contains

    subroutine error_check()

      if (x_size*y_size*z_size.ne.size(rlspace,1)) then
         ierr = 1
      elseif (x_size*y_size*z_size.ne.size(rlspace,2)) then
         ierr = 1
      elseif (size(rlspace,1).ne.size(rlspace,2)) then
         ierr = 2
      else
         ierr = 0
      endif

    end subroutine error_check

  end subroutine realspace_to_momspace

    
  subroutine fft_on_k(workreal, direction)
    type(greensfunc), intent(inout):: workreal(:, :, :, :, :, :)
    integer, intent(in)            :: direction
    
    !routine variables
    type(greensfunc), allocatable :: workslice(:, :, :)
    integer                       :: x_size, y_size, z_size, nomega
    integer                       :: ix, iy, iz
    integer                       :: ierr1

    x_size = size(workreal,1)
    y_size = size(workreal,2)
    z_size = size(workreal,3)
    nomega = size(workreal(1, 1, 1, 1, 1, 1)%GF, 1)
    
    call allocateGF(workslice, x_size, y_size, z_size, nomega, ierr1)
    if (ierr1.ne.0) call fatal_error_from_call(ierr1, "fft_on_k", "allocateGF")

    fft_each_slice_in_turn:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             call copyGF (workslice, workreal(1:x_size, 1:y_size,&
                  & 1:z_size, ix, iy, iz))
             
             call gf_fft(workslice, direction, ierr1)
             if (ierr1.ne.0) then
                write (*, *) "Slice labels (x, y, z): ", ix, iy, iz
                call fatal_error_from_call(ierr1, "fft_on_k", "gf_fft")
             endif
             
             call copyGF(workreal(1:x_size, 1:y_size, 1:z_size, ix,&
                  & iy, iz), workslice)
          enddo
       enddo
    enddo fft_each_slice_in_turn

  end subroutine fft_on_k

  subroutine fft_on_kdash(workreal, direction)
    type(greensfunc), intent(inout):: workreal(:, :, :, :, :, :)
    integer, intent(in)            :: direction
    
    !routine variables
    type(greensfunc), allocatable :: workslice(:, :, :)
    integer                       :: x_size, y_size, z_size, nomega
    integer                       :: ix, iy, iz
    integer                       :: ierr1
    
    x_size = size(workreal,4)
    y_size = size(workreal,5)
    z_size = size(workreal,6)
    nomega = size(workreal(1, 1, 1, 1, 1, 1)%GF, 1)
    
    call allocateGF(workslice, x_size, y_size, z_size, nomega, ierr1)
    if (ierr1.ne.0) then
       call fatal_error_call_from_call(ierr1, "fft_on_kdash", "allocateGF")
    end if

    fft_each_slice_in_turn:do ix= 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             call copyGF (workslice, workreal(ix, iy, iz, 1:x_size,&
                  & 1:y_size, 1:z_size))
             
             call gf_fft(workslice, direction, ierr1)
             if (ierr1.ne.0) then
                write (*, *) "Slice labels (x, y, z): ", ix, iy, iz
                call fatal_error_from_call(ierr1, "fft_on_kdash", "gf_fft")
             endif
             
             call copyGF(workreal(ix, iy, iz, 1:x_size, 1:y_size,&
                  & 1:z_size), workslice)
          enddo
       enddo
    enddo fft_each_slice_in_turn
    
  end subroutine fft_on_kdash  



  subroutine fatal_error_from_call(ierr, location, called_code_unit)
    integer, intent(in) :: ierr
    character(len=30)   :: location
    character(len=30)   :: called_code_unit

    write(*, *) "Fatal error in subroutine ", location
    write(*, *) "Call to ", called_code_unit, " failed with code ", ierr
    write(*, *) "Halting program"
    
  end subroutine fatal_error_from_call

 

  
  subroutine allocate_workreal(workreal, x_size, y_size, z_size,&
       & nomega, ierr)
    type(greensfunc), allocatable, intent(inout) :: workreal(:, :, :, :, :, :)
    integer :: x_size, y_size, z_size, nomega
    integer :: ix, iy, iz, io
    integer :: jx, jy, jz
    integer :: ierr

    call error_check()
        
    allocate(workreal(x_size, y_size, z_size,x_size, y_size, z_size))
    do ix = 1, x_size
       do iy = 1, y_size
          do iz = 1, z_size
             do jx = 1, x_size
                do jy = 1, y_size
                   do jz = 1, z_size
                      allocate(workreal(ix, iy, iz, jx, jy, jz)%GF(nomega))
                      do io = 1, nomega
                         workreal(ix, iy, iz, jx, jy, jz)%GF(io) = cmplx_zero
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
    enddo

  contains
    
    subroutine error_check()
      
      if (.not.allocated(workreal)) then
         ierr = 1
      elseif ((x_size.le.0).or.(y_size.le.0).or.(z_size.le.0)&
           & .or.(nomega.le.0)) then
         ierr = 2
      else
         ierr = 0
      endif
      
    end subroutine error_check

  end subroutine allocate_workreal


    
end module gf_fourier

