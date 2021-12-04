module kramers_kronig_relations
    use constants, only: zero, real12, cmplx_zero, cmplx_i, fatal_error_from_call, pi
    use definedtypes
    use greensroutines
    use one_dimensional_FT, only: oneD_fft, forward_fft, backward_fft, oneD_FT_initplan, oneD_FT_killplan

    implicit none
    private
    public hilbert_transform

contains

    subroutine hilbert_transform(input, result, ierr)
        ! reference: Z. Zhu, H. Yang in J. Wind Eng. Ind. Aerodyn. 90 (2002) 9–18 (operation on FFT'd components)
        ! reference: Lucas et al. AIP Advances 2, 032144 (2012) (basic form of algorithm)

        real(real12), intent(in) :: input(:)
        real(real12), intent(out) :: result(:)
        integer, intent(out) :: ierr

        complex(real12) :: full_freq(size(input))
        integer :: ierr1, i

        ierr = 0
        if (size(input) /= size(result)) ierr = 1
        !if (mod(size(input),2) /= 0) ierr = 2 
        if (ierr /= 0) return

        result = cmplx_zero
        full_freq = cmplx_zero
        full_freq(1:size(input))%re = input(1:size(input))


        call oneD_FT_initplan(full_freq, ierr1)
        if (ierr1 /= 0) call fatal_error_from_call(ierr1, 'hilbert_transform in kramers_kronig.f90', 'oneD_FT_initplan')

        call oneD_fft(full_freq, forward_fft, ierr1)
        if (ierr1 /= 0) call fatal_error_from_call(ierr1, 'hilbert_transform in kramers_kronig.f90', 'oneD_fft')
        do i = 1, size(full_freq)
            write (664, *) full_freq(i)
        end do

        if (mod(size(input),2) == 0) then
            call hilbert_even(full_freq)
        elseif (mod(size(input),2) == 1) then
            call hilbert_odd(full_freq)
        else
            call fatal_error_from_call(3, 'hilbert_transform in kramers_kronig.f90', 'size(input) neither even nor odd')
        end if

        call oneD_fft(full_freq, backward_fft, ierr1)
        if (ierr1 /= 0) call fatal_error_from_call(ierr1, 'hilbert_transform in kramers_kronig.f90', 'oneD_fft')

        call oneD_FT_killplan(ierr1)
        if (ierr1 /= 0) call fatal_error_from_call(ierr1, 'hilbert_transform in kramers_kronig.f90', 'oneD_FT_killplan')

        do i = 1, size(full_freq)
            write (665, *) full_freq(i)
        end do

        result(1:size(input)) = full_freq(1:size(input))%re

        contains

            pure subroutine hilbert_even(full_freq)
                complex(real12), intent(inout) :: full_freq(:)
                integer :: length

                length = size(full_freq)
                full_freq(1) = cmplx_zero
                full_freq(1:length/2) = -cmplx_i*full_freq(1:length/2)
                full_freq(length/2+1:length) = cmplx_i*full_freq(length/2+1:length)
            end subroutine hilbert_even

            pure subroutine hilbert_odd(full_freq)
                complex(real12), intent(inout) :: full_freq(:)
                integer :: length

                length = size(full_freq)
                full_freq(1) = cmplx_zero
                full_freq(1:(length+1)/2) = -cmplx_i*full_freq(1:(length+1)/2)
                full_freq((length+1)/2+1:length) = cmplx_i*full_freq((length+1)/2+1:length)
            end subroutine hilbert_odd

    end subroutine hilbert_transform

    impure elemental subroutine KK_Greensfunction(density, greensfunction, delta_w, ierr)
        real(real12), intent(in) :: density
        type(greensfunc), intent(out) :: greensfunction
        real(real12), intent(in) :: delta_w
        integer, intent(out) :: ierr

        integer :: no_GF_points, i, ierr1, idiv0
        real(real12), allocatable :: work_array_out(:)
        real(real12) :: temp

        ierr = 0
        idiv0 = 0

        call check_points(no_GF_points, ierr)
        if (ierr /= 0) return

        allocate (work_array_out(no_GF_points))


        call hilbert_transform(density, work_array_out, ierr1)

        do i =1, no_GF_points
            ! eqn (21) of PRB 96, 014203 (2017) lacks the frequency factor
            ! but it should be there for dimensional reasons
            output%GF(i)%re = real(i)*delta_w*work_array_out(i)
            call density_convert(density(i), temp, i, delta_w, idiv0)
            if (idiv0 /= 0 ) call fatal_error_from_call(idiv0, "KKGreensfunction in kramers_kronig.f90", "division by zero in density_convert")
            output%GF(i)%im = temp
        enddo

    contains

        subroutine check_points(number_of_points, ierr1)
            integer, intent(out) :: number_of_points
            integer, intent(inout) :: ierr1

            integer :: input_size, output_size

            number_of_points = 0
            input_size = size(density, 1)
            output_size = size(greensfunction%GF, 1)

            if (input_size /= output_size) then
                ierr1 = 1
            else
                number_of_points = input_size
            end if
        end subroutine check_points

        subroutine density_convert(density, output, i, delta_w, ierr)
            type(real12), intent(out) :: output
            real(real12), intent(in) :: density, delta_w
            integer, intent(in) :: i
            integer, intent(out) :: ierr

            ierr = 0
            if (abs(i*delta_w) < tolerance) then
                ierr = 1
                return
            endif
            output = -pi*density(i)/(two*real(i,real12)*delta_w)
        end subroutine density_convert

    end subroutine KK_Greensfunction

end module
