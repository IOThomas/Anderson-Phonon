module kramers_kronig
    use constants, only: zero, real12, cmplx_zero, cmplx_i, fatal_error_from_call
    use definedtypes
    use greensroutines
    use one_dimensional_FT, only: oneD_fft, forward_fft, backward_fft

    implicit none
    private
    public hilbert_transform

contains

    pure subroutine hilbert_transform(input, result, ierr)
        real(real12), intent(in) :: input(:)
        real(real12), intent(out) :: result(:)
        integer, intent(out) :: ierr

        complex(real12) :: full_freq(2*size(input)), positive_only(size(input))

        ierr = 0
        if (size(input) .ne. size(result)) ierr = 1
        if (ierr .ne. 0) return

        positive_only = cmplx_zero
        positive_only(1:size(input))%re = input(1:size(input))
        full_freq(1:size(input)) = conjg(positive_only(size(input):1)) ! impose negative frequency condition
        full_freq(size(input) + 1:2*size(input)) = positive_only(1:size(input))

        ! Hilbert transform (AIP Advances *2*, 032144 (2012) eqn (6))
        call oneD_fft(full_freq, forward_fft, ierr1)
        full_freq = cmplx_i*full_freq
        full_freq(1:size(input)) = -full_freq(1:size(input))
        call oneD_fft(full_freq, backward_fft, ierr1)

        result = full_freq(size(input) + 1:2*size(input))%re

    end subroutine hilbert_transform

end module
