test_suite initialisation

!global variables
integer,  parameter           :: fine_points = 6   
integer,  parameter           :: ncoarse_cells = 2
integer,  parameter           :: nomega_points= 4
integer                       :: ierr
complex(real12), parameter    :: omax = (500.0_real12, zero) 
real(real12)                  :: tol = epsilon(one)
type(settingparam)            :: settings
type(storedparam)             :: stored
type(kappagrid), allocatable  :: kgridFine(:, :, :)
type(kappagrid), allocatable  :: kgridCoarse(:, :, :)
type(greensfunc), allocatable :: Dzero(:, :, :)
type(greensfunc), allocatable :: Hybrid(:, :, :)

setup
    !code that should be run before each test
    settings%nfpoints = fine_points
    settings%ncell = ncoarse_cells
    settings%nomega = nomega_points
    settings%omegaMax = omax
    stored%omega_diff = cmplx(one, zero)
    call initgrid(settings,kgridFine,kgridCoarse,ierr)
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

end test_suite