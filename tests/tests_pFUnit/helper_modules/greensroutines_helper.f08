module greensroutines_helper
    use constants, only: real12
    use greensroutines, only: greensfunc
    implicit none

contains

    function grid_test_value_3D(greens_array, test_value)
        logical :: grid_test_value_3D
        ! dummy variables
        type(greensfunc), intent(in) :: greens_array(:,:,:)
        complex(real12), intent(in):: test_value

        integer :: dim1, dim2, dim3, nomega
        integer :: i, j, k, l
        logical :: is_problem

        dim1 = size(greens_array, 1)
        dim2 = size(greens_array, 2)
        dim3 = size(greens_array, 3)
        nomega = size(greens_array(1, 1, 1)%GF, 1)

        is_problem = .false.
        do i = 1, dim1
            if (is_problem) exit
            do j = 1, dim2
                if (is_problem) exit
                do k = 1, dim3
                    if (is_problem) exit
                    do l = 1, nomega
                        if (real(greens_array(i, j, k)%GF(l)).ne.real(test_value)) is_problem = .true.
                        if (aimag(greens_array(i, j, k)%GF(l)).ne.aimag(test_value)) is_problem = .true.
                        if (is_problem) exit
                    enddo
                enddo
            enddo
        enddo
               
        grid_test_value_3D = is_problem

    end function

end module