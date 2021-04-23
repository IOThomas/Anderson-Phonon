module one_dimensional_FT
    use constants, only: real12, zero, cmplx_zero, fatal_error_from_call
    implicit none
    include 'fftw3.f03'
    include 'fftw3q.f03'
    private
    public
    protected init_gfplan
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   useful public constants defining FFT direction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer, parameter :: forward_fft = 1
    !# use as type_flag to call a forward fft
    integer, parameter :: backward_fft = -1
    !# use as type_flag to call a backward fft
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   private variables and pointers for interoprability with C FFT routines
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(C_PTR) :: gf_plan_forward
    !stores information for forward GF FFT plan
    type(C_PTR) :: gf_plan_backward
    !stores information for backward GF FFT plan
    complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_in(:)
    complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_out(:)
    logical :: init_gfplan = .false.

contains

    subroutine 1D_FT_initplan(dimension, ierr)
        integer, intent(in) :: dimension
        integer, intent(out) :: ierr

        ierr = -1
    end subroutine 1D_FT_initplan

    subroutine 1D_FT_killplan(ierr)
        integer, intent(out) :: ierr

        ierr = -1
    end subroutine 1D_FT_killplan

    subroutine get_1D_plan_status(init_plan, forward_associated,backward_associated, work_allocated, forward_null, backward_null)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!#  Check status of various private FFT related variables
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
    end subroutine get_1D_plan_status
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine 1D_fft(transform, type_flag, ierr)
        complex(real12), intent(inout) :: transform
        integer

    end subroutine 1D_fft

end module one_dimensional_FT
  