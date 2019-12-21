test_suite initialisation

!global variables here
integer,  parameter          :: fine_points = 6   
integer,  parameter          :: ncoarse_cells = 2
integer,  parameter          :: nomega_points= 4
integer                      :: ierr
complex(real12), parameter   :: omax = (500.0_real12, zero) 
real(real12)                 :: tol = epsilon(one)
type(settingparam)           :: settings
type(storedparam)            :: stored
type(kappagrid),allocatable   :: kgridFine(:,:,:)
type(kappagrid),allocatable :: kgridCoarse(:,:,:)
type(greensfunc),allocatable :: Dzero(:,:,:)

setup
    !code that should be run before each test
    settings%nfpoints = fine_points
    settings%ncell = ncoarse_cells
    settings%nomega = nomega_points
    settings%omegaMax = omax
    call initgrid(settings,kgridFine,kgridCoarse,ierr)
end setup

teardown
    !code that runs after each test

    if (allocated(kgridFine)) deallocate(kgridFine)
    if (allocated(kgridCoarse)) deallocate(kgridCoarse)
    if (allocated(Dzero)) deallocate(Dzero)
end teardown

test nf_points_lt_one_ierr
     settings%nfpoints = -10
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
     assert_equal(ierr,1)
end test

test nomega_points_lt_one_ierr
     settings%nomega = -10
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
     assert_equal(ierr,1)
end test

test real_omega_max_lt_real_omegamin_ierr
     settings%omegamax = cmplx(-one, zero)
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
     assert_equal(ierr,2)
end test

test kgridfine_not_allocated_ierr
     deallocate(kgridfine)
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
     assert_equal(ierr,3)
end test

test dzero_already_allocated_ierr
     allocate(Dzero(1,1,1))
     call initDzero(settings,kgridFine,stored,Dzero,ierr)
     assert_equal(ierr,4)
end test

test allocate_ksize_correctly
     integer :: kfx, dzx
     integer :: kfy, dzy
     integer :: kfz, dzz
     logical :: kalloc_fail = .false.
     
     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     kfx = size(kgridFine, 1)
     kfy = size(kgridFine, 2)
     kfz = size(kgridFine, 3)

     dzx = size(Dzero, 1)
     dzy = size(Dzero, 2)
     dzz = size(Dzero, 3)

     if ((kfx.ne.dzx).or.(kfy.ne.dzy).or.(kfz.ne.dzz)) kalloc_fail = .true.
     assert_false(kalloc_fail)
end test

test allocate_nomega_correctly
     integer :: nomsz

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     nomsz = size(Dzero(1, 1, 1)%GF, 1)
     assert_equal(nomsz, nomega_points)
end test

test correct_omega_diff
     complex(real12) :: omegadiff_test
     complex(real12) :: omegamax_test

     settings%omegaMax = cmplx(one, one)
     omegadiff_test = settings%omegaMax - settings%omegaMin
     omegadiff_test = omegadiff_test/real(nomega_points)

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     assert_equal_within(real(omegadiff_test),real(stored%omega_diff),tol)
     assert_equal_within(aimag(omegadiff_test),aimag(stored%omega_diff),tol)

     omegamax_test=omegadiff_test*real(nomega_points)
     assert_equal_within(real(omegamax_test),real(settings%omegaMax),tol)
     assert_equal_within(aimag(omegamax_test),aimag(settings%omegaMax),tol)    
end test

test mappings_copied_correctly
     integer :: kgridmap
     integer :: Dzmap
     integer :: ix, iy, iz, inom
     logical :: dzmap_fail = .false.

     call initDzero(settings,kgridFine,stored,Dzero,ierr)

     do ix = 1, fine_points
     	if (dzmap_fail) exit
     	do iy = 1, fine_points
	   if (dzmap_fail) exit
	   do iz = 1, fine_points
	      if (dzmap_fail) exit
	      do inom = 1, nomega_points
	      	 kgridmap=kgridFine(ix,iy,iz)%map
		 Dzmap=Dzero(ix,iy,iz)%map
	      	 if (kgridmap.ne.Dzmap) then
		    dzmap_fail = .false.
		    exit
		 endif
	      enddo
	   enddo
	enddo
     enddo

     assert_false(dzmap_fail)
end test

end test_suite