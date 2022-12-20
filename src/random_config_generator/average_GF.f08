!------------------------------------------------------------------------------
! , Affiliation
!------------------------------------------------------------------------------
!
! MODULE:  Module name
!
!> @author
!> Author Name}
!
! DESCRIPTION: 
!>  Short module description
!
! REVISION HISTORY:
! dd Mmm yyyy - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module average_GF
    use constants, only: real12, cmplx_zero, zero, one
    use greensroutines, only: greensfunc, allocate_GF, copy_gf_slice, initialise_GF
    use config_avg, only: config_average
    use random_config, only: random_config_base, random_config_settings
    implicit none
    
contains

    subroutine get_average_GF(input_GF, output_GF, config_generator, averaging_method, n_config, frequency)
        type(greensfunc), intent(in) :: input_GF(:,:)
        type(greensfunc), intent(inout) :: output_GF(:,:)
        class(random_config_base), intent(in) :: config_generator
        class(config_average), intent(in) :: averaging_method
        complex(real12), intent(in) :: frequency(:)
        integer, intent(in) :: n_config

        complex(real12), allocatable :: random_potential(:,:)
        type(greensfunc), allocatable :: config_GF(:,:,:)
        integer :: isize, jsize, iomega, i, j, k, l, ierr

        isize = size(input_GF, 1)
        jsize = size(input_GF, 2)
        iomega = input_GF(1,1)%get_size()

        ! put error testing code here

        allocate(random_potential(isize, n_config))
        random_potential = config_generator%generate(isize, n_config)
        do i = 1, n_config
            call cluster_GF(input_GF, config_GF(1:isize,1:jsize,i), random_potential(1:isize, i), frequency, ierr)
        enddo 

        call averaging_method%calculate(config_GF,output_GF)

    end subroutine get_average_GF

    pure subroutine cluster_GF(input_GF, output_GF, potential, frequency, ierr)
        type(greensfunc), intent(in) :: input_GF(:,:)
        type(greensfunc), intent(inout) :: output_GF(:,:)
        complex(real12), intent(in) :: potential(:), frequency(:)
        integer, intent(out) :: ierr

        type(greensfunc), allocatable :: inverse_GF(:,:), inverse_cluster(:,:)
        complex(real12), allocatable :: rescale(:)
        integer :: isize, jsize, iomega, i, j, k

        ierr = 0
        isize = size(input_GF, 1)
        jsize = size(input_GF, 2)
        iomega = input_GF(1,1)%get_size()

        ! error testing code

        allocate(inverse_GF(isize,isize), inverse_cluster(isize, isize), rescale(isize))
        call allocate_GF(inverse_GF, iomega)
        call allocate_GF(inverse_cluster, iomega)

        call invert_diagonal_GF(input_GF, inverse_GF)
        call initialise_GF(inverse_cluster, cmplx_zero)
        do concurrent (i = 1:isize, j = 1:iomega)
            inverse_cluster(i,i)%GF(j) = inverse_GF(i,i)%GF(j) - frequency(j)*frequency(j)*potential(i)
        end do

        call invert_diagonal_GF(inverse_cluster, output_GF)

        rescale%re = sqrt(one - potential%re)
        rescale%im = zero

        do concurrent (i = 1:isize, j = 1:jsize, k = 1:iomega)
            output_GF(i,j)%GF(k)= rescale(i)*output_GF(i,j)%GF(k)*rescale(j)
        end do

    contains

        pure subroutine invert_diagonal_GF(input, output)
            type(greensfunc), intent(in) :: input(:,:)
            type(greensfunc), intent(out) :: output(:,:)

            integer :: ix, xsize
            complex(real12), allocatable :: slice(:,:,:)

            xsize = size(input, 1)

            allocate(slice(xsize,xsize,input(1,1)%get_size()))

            do concurrent (ix=1:input(1,1)%get_size())
                call copy_gf_slice(slice(1:xsize,1:xsize,ix), input, ix)
                slice(1:xsize,1:xsize,ix) = invert_diagonal_mat(slice(1:xsize,1:xsize,ix))
                call copy_gf_slice(output, slice(1:xsize,1:xsize,ix), ix)
            end do

        end subroutine invert_diagonal_GF

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

    end subroutine cluster_GF
    
end module average_GF