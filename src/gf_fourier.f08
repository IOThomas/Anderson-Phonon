module gf_fourier
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# This contains wrapper and related functions for FFTW routines.
!# Currently configured to use quadruple precision ('fftw3q_').
!#
!# To use: (1) call greensfunc_initplan for appropriate array size;
!#         (2) call gf_fft for those array sizes until no longer needed;
!#         (3) call greensfunc_killplan once done.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use constants, only: real12, zero, cmplx_zero, fatal_error_from_call
    use greensroutines, only: greensfunc, copy_gf_slice, allocate_GF, copyGF, initialise_gf
    use, intrinsic :: iso_c_binding
    implicit none
    include 'fftw3.f03'
    include 'fftw3q.f03'
    private
    public greensfunc_initplan, greensfunc_killplan, get_gf_plan_status, &
        gf_fft, forward_fft, backward_fft, momspace_to_realspace, realspace_to_momspace
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
    type(c_ptr) :: gf_plan_forward
    !stores information for forward GF FFT plan
    type(c_ptr) :: gf_plan_backward
    !stores information for backward GF FFT plan
    complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_in(:, :, :)
    complex(C_FLOAT128_COMPLEX), allocatable :: gfwork_out(:, :, :)
    logical :: init_gfplan = .false.
    ! have the gf plans been initialised?
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! submodule interfaces
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    interface
        module subroutine momspace_to_realspace(momentum, rlspace, ierr)
            type(greensfunc), intent(in) :: momentum(:, :, :)
            type(greensfunc), intent(inout) :: rlspace(:, :)
            integer, intent(out) :: ierr
        end subroutine
        module subroutine realspace_to_momspace(rlspace, momentum, ierr)
            type(greensfunc), intent(inout) :: momentum(:, :, :)
            type(greensfunc), intent(in) :: rlspace(:, :)
            integer, intent(out) :: ierr
        end subroutine
    end interface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
        if (ierr /= 0) return

        x_size = size(greens_function, 1)
        y_size = size(greens_function, 2)
        z_size = size(greens_function, 3)

        allocate (gfwork_in(x_size, y_size, z_size))
        allocate (gfwork_out(x_size, y_size, z_size))

        ! note ctype array ordering
        gf_plan_forward = fftwq_plan_dft_3d(z_size, y_size, x_size, gfwork_in, &
                                            gfwork_out, FFTW_FORWARD, FFTW_ESTIMATE)

        gf_plan_backward = fftwq_plan_dft_3d(z_size, y_size, x_size, gfwork_in, &
                                             gfwork_out, FFTW_BACKWARD, FFTW_ESTIMATE)

        call check_plans()
        if (ierr /= 0) return
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
            if (c_associated(gf_plan_forward, c_null_ptr)) then
                ierr = 2
            else if (c_associated(gf_plan_backward, c_null_ptr)) then
                ierr = 3
            else if ((c_associated(gf_plan_forward, c_null_ptr)) .and. &
                     (c_associated(gf_plan_backward, c_null_ptr))) then
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
        if (ierr /= 0) return

        call fftwq_destroy_plan(gf_plan_forward)
        call fftwq_destroy_plan(gf_plan_backward)

        deallocate (gfwork_in, gfwork_out)

        init_gfplan = .false.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
!------------------------------------------------------------------------------
        subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (.not. (init_gfplan)) ierr = 1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_input
!------------------------------------------------------------------------------
    end subroutine greensfunc_killplan
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine get_gf_plan_status(init_plan, forward_associated, &
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
        work_allocated = (allocated(gfwork_in)) .and. (allocated(gfwork_out))
        forward_associated = c_associated(gf_plan_forward)
        backward_associated = c_associated(gf_plan_backward)
        forward_null = c_associated(gf_plan_forward, c_null_ptr)
        backward_null = c_associated(gf_plan_backward, c_null_ptr)
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
        type(c_ptr) :: transform_type

        ierr = 0
        call check_input()
        if (ierr /= 0) return

        x_size = size(greens_function, 1)
        y_size = size(greens_function, 2)
        z_size = size(greens_function, 3)
        volume = real(x_size*y_size*z_size, real12)
        n_omega = size(greens_function(1, 1, 1)%GF, 1)
        call check_work_array_bounds()
        if (ierr /= 0) return

        if (type_flag == backward_trans) then
            transform_type = gf_plan_backward
        else if (type_flag == forward_trans) then
            transform_type = gf_plan_forward
        end if

        do iom = 1, n_omega
            call copy_gf_slice(gfwork_in, greens_function, iom)
            call fftwq_execute_dft(transform_type, gfwork_in, gfwork_out)
            !scale backwards transformation
            if (type_flag == backward_trans) gfwork_out = gfwork_out/volume
            call copy_gf_slice(greens_function, gfwork_out, iom)
        end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
!------------------------------------------------------------------------------
        subroutine check_input()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if (.not. (init_gfplan)) then
                ierr = 1
            else if ((type_flag /= backward_trans) &
                     .and. (type_flag /= forward_trans)) then
                ierr = 2
            end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_input
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
        subroutine check_work_array_bounds()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            if ((x_size /= size(gfwork_in, 1)) .or. (x_size /= size(gfwork_out, 1))) &
                then
                ierr = 3
            else if ((y_size /= size(gfwork_in, 2)) .or. &
                     (y_size /= size(gfwork_out, 2))) then
                ierr = 4
            else if ((z_size /= size(gfwork_in, 3)) .or. &
                     (z_size /= size(gfwork_out, 3))) then
                ierr = 5
            end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        end subroutine check_work_array_bounds
!------------------------------------------------------------------------------
    end subroutine gf_fft
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module gf_fourier
