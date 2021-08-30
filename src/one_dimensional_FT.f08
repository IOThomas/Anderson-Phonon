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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Sets up FFTW parameters for computations.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Error codes: ierr = 0 -- Routine executed successfully;
!#              ierr = 1 -- FFT parameters already set;
!#              ierr = 2-4 -- Null pointers for forward, backward or both FFTs
!#                       - a big problem!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        complex(real12), intent(in) :: line(:)
        integer, intent(out) :: ierr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! routine variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        integer :: x_size, y_size, z_size

        ierr = 0
        call check_input()
        if (ierr /= 0) return

        x_size = size(line, 1)

        allocate (oneDwork_in(x_size))
        allocate (oneDwork_out(x_size))

        ! note ctype array ordering
        oneD_plan_forward = fftwq_plan_dft_1d(x_size, oneDwork_in, oneDwork_out, FFTW_FORWARD, FFTW_ESTIMATE)

        oneD_plan_backward = fftwq_plan_dft_1d(x_size, oneDwork_in, oneDwork_out, FFTW_BACKWARD, FFTW_ESTIMATE)

        call check_plans()
        if (ierr /= 0) return
        init_oneDplan = .true.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
!------------------------------------------------------------------------------
        subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (init_oneDplan) ierr = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_input
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
        subroutine check_plans()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (c_associated(oneD_plan_forward, c_null_ptr)) then
                ierr = 2
            else if (c_associated(oneD_plan_backward, c_null_ptr)) then
                ierr = 3
            else if ((c_associated(oneD_plan_forward, c_null_ptr)) .and. (c_associated(oneD_plan_backward, c_null_ptr))) then
                ierr = 4
            end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_plans
!------------------------------------------------------------------------------
    end subroutine oneD_FT_initplan

    subroutine oneD_FT_killplan(ierr)
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
        if (ierr /= 0) return

        call fftwq_destroy_plan(oneD_plan_forward)
        call fftwq_destroy_plan(oneD_plan_backward)

        deallocate (oneDwork_in, oneDwork_out)

        init_oneDplan = .false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
!------------------------------------------------------------------------------
        subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (.not. (init_oneDplan)) ierr = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_input
!------------------------------------------------------------------------------
    end subroutine oneD_FT_killplan

    subroutine oneD_fft(transform, type_flag, ierr)
        complex(real12), intent(inout) :: transform(:)
        integer :: type_flag
        integer :: ierr

        ierr = -1
    end subroutine oneD_fft

end module one_dimensional_FT
