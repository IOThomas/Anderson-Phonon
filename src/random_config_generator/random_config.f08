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

    integer, parameter :: name_length = 50

    type :: random_config_settings
        character(name_length) :: generator_name
        ! for binary isotropic disorder
        
        real(real12) :: mass_0 = -1000_real12, mass_impurity = -1000_real12
        real(real12) :: impurity_concentration = -1000_real12

        ! for box disorder
        real(real12) :: disorder_strength = -1000_real12
    end type

    ! interface for base class
    type, abstract :: random_config_base
    contains
        procedure (set), public, deferred :: set
        procedure (get), public, deferred :: get
        procedure (generate), public, deferred :: generate
    end type random_config_base

    abstract interface
        subroutine set(this, settings)
            import random_config_settings, random_config_base
            type(random_config_settings), intent(in) :: settings
            class(random_config_base) :: this
        end subroutine set
        function get(this)
            import random_config_base, random_config_settings
            class(random_config_base) :: this
            type(random_config_settings) :: get
        end function get
        function generate(this, i_size, j_size) 
            import :: real12, random_config_base
            class(random_config_base) :: this
            integer, intent(in) :: i_size, j_size
            complex(real12) :: generate(i_size, j_size)
        end function generate
    end interface

    ! interfaces for extended classes
    type, extends (random_config_base) :: binary_isotopic_disorder
        real(real12), private :: mass_0, mass_impurity
        real(real12), private :: impurity_concentration
        character(50), private :: name = 'BINARY_ISOTOPIC'
    contains 
        procedure, public :: set => binary_isotopic_set
        procedure, public :: get => binary_isotopic_get
        procedure, public :: generate => binary_isotopic_generate
    end type binary_isotopic_disorder

    type, extends (random_config_base) :: box_disorder
        real(real12), private :: disorder_strength
        character(50), private :: name = 'BOX'
    contains 
        procedure, public :: set => box_set
        procedure, public :: get => box_get
        procedure, public :: generate => box_generate
    end type box_disorder

   
contains

    subroutine initialise_random_config(settings, generator, ierr)
        type(random_config_settings), intent(in) :: settings
        class(random_config_base), allocatable, intent(inout) :: generator
        integer, intent(out) :: ierr

        ierr = 0 ! assume success

        if (settings%generator_name == 'BINARY_ISOTOPIC') then
            allocate(binary_isotopic_disorder :: generator)
            call generator%set(settings)
        else if (settings%generator_name == 'BOX' ) then
            allocate (box_disorder :: generator)
            call generator%set(settings)
        else
            ierr = 1 ! incorrect generator name
        end if

    end subroutine initialise_random_config

    subroutine binary_isotopic_set(this, settings)
        class(binary_isotopic_disorder):: this
        type(random_config_settings), intent(in) :: settings

        this%mass_0 = settings%mass_0
        this%mass_impurity = settings%mass_impurity
        this%impurity_concentration = settings%impurity_concentration
    
    end subroutine binary_isotopic_set

    function binary_isotopic_get(this) result(settings)
        class(binary_isotopic_disorder) :: this
        type(random_config_settings) :: settings

        settings%generator_name = this%name
        settings%mass_0 = this%mass_0
        settings%impurity_concentration = this%impurity_concentration

    end function binary_isotopic_get

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

    subroutine box_set(this, settings)
        class(box_disorder) :: this
        type(random_config_settings), intent(in) :: settings

        this%disorder_strength = settings%disorder_strength
    
    end subroutine box_set

    function box_get(this) 
        class(box_disorder) :: this
        type(random_config_settings) :: box_get

        box_get%generator_name = this%name
        box_get%disorder_strength = this%disorder_strength

    end function box_get
        

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