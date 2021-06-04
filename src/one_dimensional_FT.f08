module one_dimensional_FT
    use constants, only: real12, zero, cmplx_zero, fatal_error_from_call
    use, intrinsic :: iso_c_binding
    implicit none
    include 'fftw3.f03'
    include 'fftw3q.f03'
    private
    public oneD_FT_initplan, oneD_FT_killplan, oneD_fft, forward_fft, backward_fft, &
        & init_oneDplan, oneDwork_in, oneDwork_out, oneD_plan_forward, oneD_plan_backward
    protected init_oneDplan, oneDwork_in, oneDwork_out, oneD_plan_forward, oneD_plan_backward
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
    type(c_ptr) :: oneD_plan_forward
    !stores information for forward one D FFT plan
    type(c_ptr) :: oneD_plan_backward
    !stores information for backward one D FFT plan
    complex(C_FLOAT128_COMPLEX), allocatable :: oneDwork_in(:)
    complex(C_FLOAT128_COMPLEX), allocatable :: oneDwork_out(:)
    logical :: init_oneDplan = .false.

contains

    subroutine oneD_FT_initplan(line, ierr)
        complex(real12), intent(in) :: line(:)
        integer, intent(out) :: ierr

        ierr = -1
    end subroutine oneD_FT_initplan

    subroutine oneD_FT_killplan(ierr)
        integer, intent(out) :: ierr

        ierr = -1
    end subroutine oneD_FT_killplan

    subroutine oneD_fft(transform, type_flag, ierr)
        complex(real12), intent(inout) :: transform(:)
        integer :: type_flag
        integer :: ierr

        ierr = -1
    end subroutine oneD_fft

end module one_dimensional_FT
