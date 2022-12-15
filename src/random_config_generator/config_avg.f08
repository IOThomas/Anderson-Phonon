!------------------------------------------------------------------------------
! Institution, Affiliation
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
module config_avg
    use constants, only: real12, cmplx_zero, fatal_error_from_call
    use greensroutines, only: greensfunc, copy_gf_slice, allocate_GF, is_GF_allocated

    private
    public initialise_average, config_average, standard_average
    
    ! interface for base class
    type, abstract :: config_average
    contains
        procedure (calculate), public, deferred:: calculate
    end type config_average

    abstract interface
        subroutine calculate(this, greensfunctions, result)
            import config_average, greensfunc
            class(config_average), intent(in) :: this
            type(greensfunc), intent(in) :: greensfunctions(:,:,:)
            type(greensfunc), intent(inout) :: result(:,:)
        end subroutine calculate
    end interface

    ! interface for extented class
    type, extends (config_average) :: standard_average
    contains
        procedure, public :: calculate => standard_calculate
    end type standard_average

contains

    subroutine initialise_average(selection, calculator, ierr)
        character(50), intent(in) :: selection
        class(config_average), allocatable, intent(inout) :: calculator
        integer, intent(out) :: ierr

        ierr = 0
        if (selection == 'STANDARD') then 
            allocate(standard_average :: calculator)
        else 
            ierr = 1
        end if

    end subroutine initialise_average

    subroutine standard_calculate(this, greensfunctions, result)
        class(standard_average), intent(in) :: this
        type(greensfunc), intent(in) :: greensfunctions(:,:,:)
        type(greensfunc), intent(inout):: result(:,:)

        integer :: isize, jsize, n_config, n_layer
        integer :: i, j 
        logical :: array_size_match, test_allocated
        complex(real12), allocatable :: total_layer(:,:,:), current_layer(:,:,:,:)

        array_size_match = greensfunctions(1,1,1)%get_size() == result(1,1)%get_size()
        if (.not. array_size_match) &
            & call fatal_error_from_call(1,'config_avg.f90','standard_calculate')
        array_size_match = (size(greensfunctions,1) == size(result,1)) .and.(size(greensfunctions,2) == size(result,2))
        if (.not. array_size_match) &
            & call fatal_error_from_call(2,'config_avg.f90','standard_calculate')
        test_allocated = all(is_GF_allocated(greensfunctions)) .and. all(is_GF_allocated(result))
        if (.not. test_allocated) &
            & call fatal_error_from_call(3, 'config_avg.f90', 'standard_calculate')

        isize = size(greensfunctions,1)
        jsize = size(greensfunctions,2)
        n_config = size(greensfunctions,3)
        n_layer = greensfunctions(1,1,1)%get_size()
        allocate(total_layer(isize, jsize, n_layer), current_layer(isize,jsize,n_layer, n_config))
        call allocate_GF(result, n_layer)
        total_layer = cmplx_zero

        do concurrent(i=1:n_layer, j=1:n_config)
            call copy_gf_slice(current_layer(1:isize,1:jsize, i,j), greensfunctions(1:isize,1:jsize,j), i)
        end do
    

        do concurrent (i = 1:n_layer)
            do j = 1, n_config
                total_layer(1:isize, 1: jsize, i) = total_layer(1:isize, 1: jsize, i) + current_layer(1:isize,1:jsize, i, j)
            end do

            total_layer(1:isize,1:jsize,i) = total_layer(1:isize,1:jsize,i)/real(n_config,real12)

            call copy_gf_slice(result, total_layer(1:isize,1:jsize,i), i)
        end do


    end subroutine standard_calculate


end module config_avg
