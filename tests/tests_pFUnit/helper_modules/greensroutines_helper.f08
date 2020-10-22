module greensroutines_helper
    use constants, only: real12
    use greensroutines, only: greensfunc
    implicit none

contains

    elemental function does_GF_have_problem(greens_array, test_value)
        logical :: does_GF_have_problem
        ! dummy variables
        type(greensfunc), intent(in)  :: greens_array
        complex(real12), intent(in)   :: test_value

        integer :: iomega, nomega
        
        nomega = size(greens_array%GF, 1)

        does_GF_have_problem = .false.
        do iomega = 1, nomega
            if (real(greens_array%GF(iomega)) .ne. real(test_value)) does_GF_have_problem = .true.
            if (aimag(greens_array%GF(iomega)) .ne. aimag(test_value)) does_GF_have_problem = .true.
            if (does_GF_have_problem) exit
        end do

    end function

    elemental function  does_map_have_problem(greens_array, test_value)
        logical :: does_map_have_problem
        ! dummy variables
        type(greensfunc), intent(in)  :: greens_array
        integer, intent(in) :: test_value

        if (greens_array%map == test_value) then
            does_map_have_problem = .false.
        else
            does_map_have_problem = .true.
        end if
        
    end function

end module