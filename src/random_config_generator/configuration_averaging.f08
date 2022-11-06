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
module configuration_averaging
    use constants, only: real12, cmplx_zero
    use greensroutines, only: greensfunc, copy_gf_slice

    private
    public config_average, standard_average
    
    ! interface for base class
    type, abstract :: config_average
    contains
        procedure (calculate), public, deferred:: calculate
    end type config_average

    abstract interface
        pure function calculate(this, greensfunctions)
            import config_average, greensfunc
            class(config_average), intent(in) :: this
            type(greensfunc), intent(in) :: greensfunctions(:,:,:)
            type(greensfunc) :: calculate(size(greensfunctions,1),size(greensfunctions,2))
        end function calculate
    end interface

    ! interface for extented class
    type, extends (config_average) :: standard_average
    contains
        procedure, public :: calculate => standard_calculate
    end type standard_average

contains

    pure function standard_calculate(this, greensfunctions)
        class(standard_average), intent(in) :: this
        type(greensfunc), intent(in) :: greensfunctions(:,:,:)
        type(greensfunc) :: standard_calculate(size(greensfunctions,1),size(greensfunctions,2))

        integer :: isize, jsize, n_config, n_layer
        integer :: i, j, k 
        complex(real12), allocatable :: total_layer(:,:,:), current_layer(:,:,:,:)

        isize = size(greensfunctions,1)
        jsize = size(greensfunctions,2)
        n_config = size(greensfunctions,3)
        n_layer = greensfunctions(1,1,1)%get_size()
        allocate(total_layer(isize, jsize, n_layer), current_layer(isize,jsize,n_layer, n_config))

        total_layer = cmplx_zero

        do concurrent(i=1:n_layer, j=1:n_config)
            call copy_gf_slice(current_layer(1:isize,1:jsize, i,j), greensfunctions(1:isize,1:jsize,j), i)
        end do

        do concurrent (i = 1:n_layer)
            do j = 1, n_config
                total_layer(1:isize, 1: jsize, i) = total_layer(1:isize, 1: jsize, i) + current_layer(1:isize,1:jsize, i, j)
            end do

            total_layer(1:isize,1:jsize,i) = total_layer(1:isize,1:jsize,i)/real(n_config,real12)

            call copy_gf_slice(standard_calculate, total_layer(1:isize,1:jsize,i), i)
        end do


    end function standard_calculate


end module configuration_averaging
