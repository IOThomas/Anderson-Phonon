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

    type, abstract :: random_config_base
    contains
        procedure, deferred :: generate
    end type random_config_base

    abstract interface
        function generate(i_size, j_size)
            integer, intent(in) :: i_size, j_size
            complex(real12) :: generate(i_size, j_size)
        end function generate
    end interface

    type, extends (random_config_base) :: binary_isotopic_disorder
        real(real12) :: mass_0, mass_impurity
        real(real12) :: impurity_concentration
    contains 
        procedure :: generate => binary_isotopic_generate
    end type binary_disorder

    type, extends (random_config_base) :: box_disorder
        real(real12) :: disorder_strength
    contains 
        procedure :: generate => box_generate
    end type box_disorder

contains

    function binary_isotopic_generate(i_size, j_size)
        integer, intent(in) :: i_size, j_size
        complex(real12) :: binary_isotopic_generate(i_size, j_size)

        integer :: i
        real(real12) :: random_sequence(i_size)

        call random_number(random_sequence)

        binary_isotopic_generate = cmplx_zero

        do contiguous (i=1:i_size)
            if (random_sequence(i) < impurity_concentration) then
                binary_isotopic_generate(i,i)%re = one - mass_impurity/mass_0
            end if
        end do

    end function binary_isotopic_disorder

    function box_generate(i_size, j_size)
        integer, intent(in) :: i_size, j_size
        complex(real12) :: box_generate(i_size, j_size)

        integer :: i
        real(real12) :: random_sequence(i_size)

        call random_number(random_sequence)

        box_generate = zero

        do contiguous (i=1:i_size)
            ! here we make sure that 1 >= V > 0
            box_generate(i,i) = two*(one - random_sequence(i))*impurity_concentration - impurity_concentration
        end do
    end function box_generate



end module random_config