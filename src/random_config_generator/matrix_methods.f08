module matrix_methods
    use constants
    use greensroutines
    implicit none

    !external CGETRI
    !external CGETRF
    interface ! declare external LAPACK routines 
        subroutine CGETRF(n_row, n_col, matrix_A, lead_dim_A, pivot_indices, ierror)
            import real12
            integer :: ierror, lead_dim_A, n_row, n_col
            integer :: pivot_indices(*)
            complex(real12) :: matrix_A(lead_dim_A, *)
        end subroutine CGETRF
        subroutine CGETRI(n_row, matrix_A, lead_dim_A, pivot_indices, work_matrix,  work_dim, ierror)
            import real12
            integer :: ierror, lead_dim_A, work_dim, n_row
            integer :: pivot_indices(*)
            complex(real12) :: matrix_A(lead_dim_A,*), work_matrix(*)
        end subroutine
    end interface
    
contains

    subroutine invert_GF_matrix(input, output)
        type(greensfunc), intent(in) :: input(:,:)
        type(greensfunc), intent(out) :: output(:,:)

        integer :: ix, xsize
        complex(real12), allocatable :: slice(:,:,:)
        logical, allocatable :: is_diagonal(:)

        xsize = size(input, 1)

        allocate(slice(xsize,xsize,input(1,1)%get_size()), is_diagonal(input(1,1)%get_size()))

        do concurrent (ix=1:input(1,1)%get_size())
            call copy_gf_slice(slice(1:xsize,1:xsize,ix), input, ix)

            is_diagonal(ix) = diagonal_test(slice(1:xsize,1:xsize, ix))
            if (is_diagonal(ix)) then
                slice(1:xsize,1:xsize,ix) = invert_diagonal_mat(slice(1:xsize,1:xsize,  ix))
            else
                !slice(1:xsize,1:xsize,ix) = invert_general_matrix(slice(1:xsize,1:xsize,  ix))
            end if
            call copy_gf_slice(output, slice(1:xsize,1:xsize,ix), ix)
        end do

    contains
        
        pure function diagonal_test(matrix)
            complex(real12), intent(in) :: matrix(:,:)
            logical :: diagonal_test

            logical :: not_zero(size(matrix,1), size(matrix,2))
            integer :: i, j

            not_zero = .false.

            do concurrent(i=1:size(matrix,1), j=1:size(matrix,2))
                if ((i /= j) .and. (matrix(i,j) == cmplx_zero)) not_zero = .true.
            end do

            diagonal_test = any(not_zero)

        end function diagonal_test

    end subroutine invert_GF_matrix

    pure function invert_diagonal_mat(matrix)
        complex(real12), intent(in) :: matrix(:,:)
        complex(real12) :: invert_diagonal_mat(size(matrix,1),size(matrix,2))

        integer :: ix

        invert_diagonal_mat = cmplx_zero
        do concurrent (ix=1:size(matrix,1))
            if (matrix(ix,ix) /= cmplx_zero) then
                invert_diagonal_mat(ix,ix) = 1.0/matrix(ix,ix)
            else
                invert_diagonal_mat(ix,ix)%re = huge(real12)
                invert_diagonal_mat(ix,ix)%im = zero
            end if
        end do

    end function invert_diagonal_mat


    function invert_general_matrix(matrix)
        complex(real12), intent(in) :: matrix(:,:)
        complex(real12) :: invert_general_matrix(size(matrix, 1), size(matrix, 2))

        integer :: lda, n_rows, ierr, pivots(max(size(matrix,1), size(matrix,2))), lwork
        complex(real12), allocatable :: work(:)

        call check_inputs()

        n_rows = size(matrix, 1)
        invert_general_matrix = matrix
        lda = max(1,n_rows)
        lwork = lda
        allocate(work(lwork))
        ierr = 0

        call CGETRF(n_rows, n_rows, invert_general_matrix, lda, pivots, ierr)

        if (ierr /= 0) call fatal_error_from_call(ierr, 'invert_general_matrix', 'matrix_methods.f08')

        call CGETRI(n_rows, invert_general_matrix, lda, pivots, work, lwork, ierr)

        if (ierr /= 0) call fatal_error_from_call(ierr, 'invert_general_matrix', 'matrix_methods.f08')

    contains

        subroutine check_inputs()
            if (size(matrix,1) /= size(matrix,2)) call fatal_error_from_call(0, 'invert_general_matrix', 'matrix_methods.f08')    
        end subroutine  check_inputs

    end function invert_general_matrix
    
end module matrix_methods
