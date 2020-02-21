test_suite initialisation

!global variables
integer,  parameter           :: fine_points = 4   
integer,  parameter           :: ncoarse_cells = 2
integer,  parameter           :: nomega_points= 4
integer                       :: ierr
complex(real12), parameter    :: omax = cmplx(500.0_real12, zero) 
type(settingparam)            :: settings
type(storedparam)             :: stored
type(kappagrid), allocatable  :: kgridFine(:, :, :)
type(kappagrid), allocatable  :: kgridCoarse(:, :, :)
type(greensfunc), allocatable :: Dzero(:, :, :)
type(greensfunc), allocatable :: Hybrid(:, :, :)

setup
    integer :: i, j, k
    !code that should be run before each test
    settings%nfpoints = fine_points
    settings%ncell = ncoarse_cells
    settings%nomega = nomega_points
    settings%omegaMax = omax
    stored%omega_diff = cmplx(one, zero)
    allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
    do i = 1, ncoarse_cells
        do j = 1, ncoarse_cells
	     do k = 1, ncoarse_cells
	             kgridCoarse(i, j, k)%kx = one
	             kgridCoarse(i, j, k)%ky = one
	             kgridCoarse(i, j, k)%kz = one
	             kgridCoarse(i, j, k)%norm = one
	             kgridCoarse(i, j, k)%map = i + (j - 1)*ncoarse_cells&
	                  + (k - 1)*ncoarse_cells*ncoarse_cells
             enddo
        enddo
    enddo

    call allocateGF(Dzero, fine_points, fine_points, fine_points, &
        nomega_points, ierr)
end setup

teardown
    !code that runs after each test

    if (allocated(kgridFine)) deallocate(kgridFine)
    if (allocated(kgridCoarse)) deallocate(kgridCoarse)
    if (allocated(Dzero)) deallocate(Dzero)
    if (allocated(Hybrid)) deallocate(Hybrid)
   
end teardown

test storedomega_ierr_1

! these should give ierr = 1
     stored%omega_diff = cmplx(zero, zero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 1)

     stored%omega_diff = cmplx(zero, one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 1)
     

     stored%omega_diff = cmplx(zero, -one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 1)

     stored%omega_diff = cmplx(-one, zero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 1)
     
     stored%omega_diff = cmplx(-one, -one)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 1)    
     

end test

test kgrid_deallocated_ierr2

     deallocate(kgridCoarse)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 2)

end test

test Dzero_deallocated_ierr2

     deallocate(Dzero)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 2)

end test

test Dzero_kgrid_deallocated_ierr2

     deallocate(Dzero)
     deallocate(kgridCoarse)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 2)

end test

test Gzero_allocated_ierr3

     call allocateGF(Hybrid, ncoarse_cells, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 3)

end test

test finepoints_le_coarsepoints_ierr4

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, fine_points, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
     
     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, fine_points, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, ncoarse_cells, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
     
     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, ncoarse_cells-1, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, fine_points, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, fine_points, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, ncoarse_cells, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, ncoarse_cells-1, fine_points, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, fine_points, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
     
     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, fine_points, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, fine_points, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero)
     call allocateGF(Dzero, fine_points, ncoarse_cells-1, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero, kgridCoarse)
     allocate(kgridCoarse(ncoarse_cells, ncoarse_cells, ncoarse_cells))
     call allocateGF(Dzero, ncoarse_cells, ncoarse_cells, ncoarse_cells, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)

     deallocate(Dzero)
     call allocateGF(Dzero, ncoarse_cells-1, ncoarse_cells-1, ncoarse_cells-1, &
        nomega_points, ierr)
     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)
     assert_equal(ierr, 4)
end test

test output_eqn
     integer      :: i, j, k, l
     integer      :: ic, jc, kc
     real(real12) :: real_output(4) = (/ -1.0625_real12, 1.9375_real12, &
     		  6.9375_real12, 13.9375_real12 /)
     real(real12) :: imag_output = -1.9375_real12
     logical      :: output_problem = .false.
     logical      :: mapping_problem = .false.


! initialisation
     do i = 1, ncoarse_cells
     	do j = 1, ncoarse_cells
	   do k = 1, ncoarse_cells
	      do l = 1, nomega_points
	      	 kgridCoarse(i, j, k)%omega2 = cmplx(2.0_real12, 2.0_real12)
	      enddo
	   enddo
	enddo
     enddo

     do i = 1, fine_points
        do j = 1, fine_points
           do k = 1, fine_points
	      ic = nint(real(i)/two)
              jc = nint(real(j)/two)
              kc = nint(real(k)/two)
              Dzero(i , j , k)%map = ic + (jc - 1)*ncoarse_cells &
                  + (kc - 1)*ncoarse_cells*ncoarse_cells
              do l = 1, nomega_points
                 Dzero(i, j, k)%GF(l) = cmplx(one, one)
              enddo
           enddo
        enddo
     enddo

     call initHybrid(stored, kgridCoarse, Dzero, Hybrid, ierr)

! checks
     do i = 1, ncoarse_cells
     	if (output_problem) exit
     	do j = 1, ncoarse_cells
	   if (output_problem) exit
	   do k = 1, ncoarse_cells
	      if (output_problem) exit
	      do l = 1, nomega_points
                 if (real(Hybrid(i, j, k)%GF(l)).ne.real_output(l)) then
		    output_problem = .true.
		    exit
		 endif
		 if (aimag(Hybrid(i, j, k)%GF(l)).ne.imag_output) then
		    output_problem = .true.
		    exit
		 endif
              enddo
	   enddo
	enddo
     enddo
     assert_false(output_problem)
 
     do i = 1, ncoarse_cells
     	if (mapping_problem) exit
     	do j = 1, ncoarse_cells
	   if (mapping_problem) exit
	   do k = 1, ncoarse_cells
	      if (Hybrid(i, j, k)%map.ne.kgridCoarse(i, j, k)%map) then
	      	 mapping_problem = .true.
		 exit
	      endif
	   enddo
	enddo
     enddo
     assert_false(mapping_problem)

end test
     

end test_suite