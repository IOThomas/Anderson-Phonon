module gf_fourier

  ! currently configured to use quadruple precision ('fftw3q_') routines
  use constants, only: real12
  use greensroutines, only: greensfunc
  use, intrinsic :: iso_c_binding
  implicit none
  include 'fftw3.f03'
  include 'fftw3q.f03'
  private
  public greensfunc_initplan, greensfunc_killplan, get_gf_plan_status, &
       gf_fft

  type(C_PTR) :: gf_plan_forward
  !stores information for forward GF FFT plan
  type(C_PTR) :: gf_plan_backward
  !stores information for backward GF FFT plan
  complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_in(:,:,:)
  complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_out(:,:,:)
  logical :: init_gfplan = .false.
  ! have the gf plans been initialised

contains

  subroutine greensfunc_initplan(greens_function, ierr)
    type(greensfunc), intent(in) :: greens_function(:, :, :)
    integer, intent(out)         :: ierr  

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
    
  contains

    subroutine check_input()
      
      if (init_gfplan) ierr = 1

    end subroutine check_input

    subroutine check_plans()

      if (c_associated(gf_plan_forward, C_NULL_PTR)) then
         ierr = 2
      else if (c_associated(gf_plan_backward, C_NULL_PTR)) then
         ierr = 3
      else if ((c_associated(gf_plan_forward, C_NULL_PTR)).and.&
           (c_associated(gf_plan_backward, C_NULL_PTR))) then
         ierr = 4
      end if

    end subroutine check_plans

  end subroutine greensfunc_initplan

  subroutine greensfunc_killplan(ierr)
    integer, intent(out)   :: ierr

    ierr = 0
    call check_input()
    if (ierr.ne.0) return

    call fftwq_destroy_plan(gf_plan_forward)
    call fftwq_destroy_plan(gf_plan_backward)

    deallocate(gfwork_in, gfwork_out)

    init_gfplan = .false.

  contains
    
    subroutine check_input()

      if (.not.(init_gfplan)) ierr = 1
    end subroutine check_input
    
  end subroutine greensfunc_killplan

  subroutine get_gf_plan_status(init_plan, forward_associated,&
       backward_associated, work_allocated, forward_null, backward_null)
    logical, intent(out) :: init_plan
    logical, intent(out) :: forward_associated
    logical, intent(out) :: backward_associated
    logical, intent(out) :: work_allocated
    logical, intent(out) :: forward_null
    logical, intent(out) :: backward_null

    init_plan = init_gfplan
    work_allocated = (allocated(gfwork_in)).and.(allocated(gfwork_out))
    forward_associated = c_associated(gf_plan_forward)
    backward_associated = c_associated(gf_plan_backward)
    forward_null = c_associated(gf_plan_forward, C_NULL_PTR)
    backward_null = c_associated(gf_plan_backward, C_NULL_PTR)
 
  end subroutine get_gf_plan_status
    

  subroutine gf_fft(greens_function, type_flag, ierr)
    type(greensfunc), intent(inout) :: greens_function(:, :, :)
    integer, intent(in) :: type_flag !must be -1 or +1
    integer, intent(out) :: ierr

    integer :: x_size, y_size, z_size, n_omega
    integer :: ix, iy, iz, iom
    type(C_PTR) :: transform_type

    ierr = 0
    call check_input()
    if (ierr.ne.0) return

    x_size = size(greens_function, 1)
    y_size = size(greens_function, 2)
    z_size = size(greens_function, 3)
    n_omega = size(greens_function(1, 1, 1)%GF, 1)
    call check_work_array_bounds()
    if (ierr.ne.0) return

    if (type_flag.eq.-1) then
       transform_type = gf_plan_backward
    else if (type_flag.eq.1) then
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
                greens_function(ix, iy, iz)%GF(iom)=gfwork_out(ix, iy, iz)
             enddo
          enddo
       enddo
    enddo

  contains

    subroutine check_input()

      if (.not.(init_gfplan)) then
         ierr = 1
      else if ((type_flag.ne.-1).and.(type_flag.ne.1)) then
         ierr = 2
      end if
      
    end subroutine check_input

    subroutine check_work_array_bounds()

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

    end subroutine check_work_array_bounds

  end subroutine gf_fft

end module gf_fourier

