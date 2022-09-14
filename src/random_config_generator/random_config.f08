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
module random_config
    use constants, only: real12, one, cmplx_zero, zero, two
    use definedtypes
    implicit none
    private
    public random_config_settings, initialise_random_config, random_config_base, binary_isotopic_disorder, box_disorder

    type :: random_config_settings
        ! for binary isotropic disorder
        ! possibly initialise as NaN's so can check initialisation once class variables are made private?
        real(real12) :: mass_0 = one, mass_impurity = one
        real(real12) :: impurity_concentration = zero

        ! for box disorder
        real(real12) :: disorder_strength = one
    end type

    ! interface for base class
    type, abstract :: random_config_base
    contains
        procedure (generate), public, deferred:: generate
    end type random_config_base

    abstract interface
        function generate(this, i_size, j_size) 
            import :: real12, random_config_base
            class(random_config_base) :: this
            integer, intent(in) :: i_size, j_size
            complex(real12) :: generate(i_size, j_size)
        end function generate
    end interface

    ! interfaces for extended classes
    type, extends (random_config_base) :: binary_isotopic_disorder
        real(real12) :: mass_0, mass_impurity
        real(real12) :: impurity_concentration
    contains 
        procedure, public :: generate => binary_isotopic_generate
    end type binary_isotopic_disorder

    type, extends (random_config_base) :: box_disorder
        real(real12) :: disorder_strength
    contains 
        procedure, public :: generate => box_generate
    end type box_disorder

   
contains

    pure subroutine initialise_random_config(generator_name, settings, generator, ierr)
        character(*), intent(in):: generator_name
        type(random_config_settings), intent(in) :: settings
        class(random_config_base), allocatable, intent(inout) :: generator
        integer, intent(out) :: ierr

        ierr = 0 ! assume success

        if (generator_name == 'BINARY_ISOTOPIC') then
            allocate(binary_isotopic_disorder :: generator)
            generator = binary_isotopic_disorder(settings%mass_0, settings%mass_impurity, settings%impurity_concentration)
        else if (generator_name == 'BOX' ) then
            allocate (box_disorder :: generator)
            generator = box_disorder(settings%disorder_strength)
        else
            ierr = 1 ! incorrect generator name
        end if

    end subroutine initialise_random_config

    function binary_isotopic_generate(this, i_size, j_size)
        class(binary_isotopic_disorder) :: this
        integer, intent(in) :: i_size, j_size
        complex(real12) :: binary_isotopic_generate(i_size, j_size)

        integer :: i
        real(real12) :: random_sequence(i_size)

        call random_number(random_sequence)

        binary_isotopic_generate = cmplx_zero

        do concurrent (i=1:i_size)
            if (random_sequence(i) < this%impurity_concentration) then
                binary_isotopic_generate(i,i)%re = one - this%mass_impurity/this%mass_0
            end if
        end do

    end function binary_isotopic_generate

    function box_generate(this, i_size, j_size)
        class(box_disorder) :: this
        integer, intent(in) :: i_size, j_size
        complex(real12) :: box_generate(i_size, j_size)

        integer :: i
        real(real12) :: random_sequence(i_size)

        call random_number(random_sequence)

        box_generate = cmplx_zero

        do concurrent (i=1:i_size)
            ! here we make sure that 1 >= V > 0
            box_generate(i,i)%re = two*(one - random_sequence(i))*this%disorder_strength - this%disorder_strength
        end do
    end function box_generate

end module random_config