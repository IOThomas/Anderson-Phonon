module matrix_methods
    use constants
    use greensroutines
    implicit none
    private
    public invert_GF_matrix, invert_diagonal_mat, invert_general_matrix

    !external ZGETRI
    !external ZGETRF
    interface ! declare external LAPACK routines 
        subroutine ZGETRF(n_row, n_col, matrix_A, lead_dim_A, pivot_indices, ierror)
            import real12
            integer :: ierror, lead_dim_A, n_row, n_col
            integer :: pivot_indices(*)
            complex(real12) :: matrix_A(lead_dim_A, *)
        end subroutine ZGETRF
        subroutine ZGETRI(n_row, matrix_A, lead_dim_A, pivot_indices, work_matrix,  work_dim, ierror)
            import real12
            integer :: ierror, lead_dim_A, work_dim, n_row
            integer :: pivot_indices(*)
            complex(real12) :: matrix_A(lead_dim_A,*), work_matrix(*)
        end subroutine ZGETRI
    end interface
    
contains

    subroutine invert_GF_matrix(input, output)
        type(greensfunc), intent(in) :: input(:,:)
        type(greensfunc), intent(out) :: output(:,:)

        integer :: ix, xsize
        integer, allocatable :: ierror(:)
        complex(real12), allocatable :: slice(:,:,:)
        logical, allocatable :: is_diagonal(:)

        call check_inputs()

        xsize = size(input, 1)

        allocate(slice(xsize,xsize,input(1,1)%get_size()), is_diagonal(input(1,1)%get_size()), ierror(input(1,1)%get_size()))

        do ix = 1, input(1,1)%get_size()
            call copy_gf_slice(slice(1:xsize,1:xsize,ix), input, ix)

            is_diagonal(ix) = diagonal_test(slice(1:xsize,1:xsize, ix))
            if (is_diagonal(ix)) then
                call invert_diagonal_mat(slice(1:xsize,1:xsize,ix), slice(1:xsize,1:xsize,ix), ierror(ix))
            else
                call invert_general_matrix(slice(1:xsize,1:xsize,ix), slice(1:xsize,1:xsize,ix), ierror(ix))
                
            end if
            call copy_gf_slice(output, slice(1:xsize,1:xsize,ix), ix)
        end do

        if (any(ierror /= 0 )) call handle_errors()

    contains

        
        subroutine check_inputs()
            if (size(input,1) /= size(input,2)) call fatal_error_from_call(1,  'invert_GF_matrix', 'matrix_methods.f08')   
            if (size(output,1) /= size(output,2)) call fatal_error_from_call(2, 'invert_GF_matrix', 'matrix_methods.f08') 
            if (size(input,1) /= size(output,1)) call fatal_error_from_call(3, 'invert_GF_matrix', 'matrix_methods.f08')
        end subroutine  check_inputs

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

        subroutine handle_errors()
            integer :: slice_no, error_val
            logical, allocatable :: error_mask(:)

            allocate(error_mask(size(ierror,1)))
            error_mask = .false.
            where (ierror /= 0) error_mask = .true.
            slice_no = findloc(error_mask, .true.,1)
            error_val = ierror(slice_no)

            if (error_val < 0) then
                write(*,*) "Error in inverting matrix for Green's function momentum slice ", slice_no
                write(*,*) "Argument ", error_val, "of one of the LAPACK routines has an illegal value."
                call fatal_error_from_call(4,'invert_GF_matrix', 'matrix_methods.f08') 
            else if ((error_val > 0) .and. (.not. is_diagonal(slice_no))) then
                write(*,*) "Error in inverting matrix for Green's function momentum slice ", slice_no
                write(*,*) "U(", error_val, ",", error_val, ") is exactly zero following decomposition by LAPACK routines."
                write(*,*) "Singular matrix: cannot be inverted."
                call fatal_error_from_call(5,'invert_GF_matrix', 'matrix_methods.f08') 
            else if ((error_val > 0) .and. (is_diagonal(slice_no))) then
                write(*,*) "Error in inverting matrix for Green's function momentum slice ", slice_no
                write(*,*) "Component (", error_val, ",", error_val, ") of the matrix is exactly zero."
                write(*,*) "Singular matrix: cannot be inverted."
                call fatal_error_from_call(6,'invert_GF_matrix', 'matrix_methods.f08') 
            end if 
        end subroutine handle_errors

    end subroutine invert_GF_matrix

    pure subroutine invert_diagonal_mat(matrix, inverse, ierr)
        complex(real12), intent(in) :: matrix(:,:)
        complex(real12), intent(out) :: inverse(:,:)
        integer, intent(out) :: ierr

        integer :: ix, iflag(size(matrix, 1))

        ierr = 0
        inverse = cmplx_zero
        iflag = 0
        do concurrent (ix=1:size(matrix,1))
            if (matrix(ix,ix) /= cmplx_zero) then
                inverse(ix,ix) = 1.0/matrix(ix,ix)
            else
                inverse(ix,ix) = matrix(ix, ix)
                iflag(ix) = 1
            end if
        end do

        if (any(iflag == 1)) ierr = findloc(iflag, 1, 1)

    end subroutine invert_diagonal_mat


    subroutine invert_general_matrix(matrix, inverse, ierr)
        complex(real12), intent(in) :: matrix(:,:)
        complex(real12), intent(out) :: inverse(:, :)
        integer, intent(out) :: ierr

        integer :: lda, n_rows, pivots(max(size(matrix,1), size(matrix,2))), lwork
        complex(real12), allocatable :: work(:)

        

        n_rows = size(matrix, 1)
        inverse = matrix
        lda = max(1,n_rows)
        lwork = lda
        allocate(work(lwork))
        ierr = 0

        call ZGETRF(n_rows, n_rows, inverse, lda, pivots, ierr)

        if (ierr /= 0) return

        call ZGETRI(n_rows, inverse, lda, pivots, work, lwork, ierr)

        if (ierr /= 0) return


    end subroutine invert_general_matrix
    
end module matrix_methods
