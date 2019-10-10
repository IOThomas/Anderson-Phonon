test_suite initialisation

!global variables here (don't change fine_points or ncoarse_cells)
integer,  parameter :: fine_points=6   
integer,  parameter :: ncoarse_cells=2
integer             :: ierr
type(settingparam)  :: settings
type(finegrid),allocatable     :: kgridFine(:,:,:)
type(coarsegrid),allocatable   :: kgridCoarse(:,:,:)


setup              
  !code that should be run before each test
  settings%nfpoints=fine_points
  settings%ncell=ncoarse_cells
end setup

teardown
  !code runs after each test
  integer::stat

  if (allocated(kgridFine)) deallocate(kgridFine)
  if (allocated(kgridCoarse)) deallocate(kgridCoarse)
end teardown

test negative_finegrid_ierr
  settings%nfpoints(3)=-1
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(ierr,1)
end test

test odd_finegrid_ierr
  settings%nfpoints(3)=5
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(ierr,2)
end test

test negative_coarsegrid_ierr
  settings%ncell(3)=-1
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(ierr,1)
end test


test odd_coarsegrid_ierr
  settings%ncell(3)=5
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(ierr,2)
end test

test fine_points_stored_correctly
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(settings%nfpoints(1),fine_points)
  assert_equal(settings%nfpoints(2),fine_points)
  assert_equal(settings%nfpoints(3),fine_points)
  
end test

test coarse_cell_no_stored_correctly
  call initgrid(settings,kgridFine,kgridCoarse,ierr)
  assert_equal(settings%ncell(1),ncoarse_cells)
  assert_equal(settings%ncell(2),ncoarse_cells)
  assert_equal(settings%ncell(3),ncoarse_cells)
  
end test

test fine_grid_array_size
  integer:: ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  ix=size(kgridFine,1)
  iy=size(kgridFine,2)
  iz=size(kgridFine,3)

  assert_equal(ix,fine_points)
  assert_equal(iy,fine_points)
  assert_equal(iz,fine_points)
  
end test

test coarse_grid_array_size
  integer:: ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  ix=size(kgridCoarse,1)
  iy=size(kgridCoarse,2)
  iz=size(kgridCoarse,3)

  assert_equal(ix,ncoarse_cells)
  assert_equal(iy,ncoarse_cells)
  assert_equal(iz,ncoarse_cells)
  
end test

test fine_grid_values_corners
  real(real12),parameter::first=-pi/3.0_real12
  real(real12),parameter::last=4.0_real12*pi/3.0_real12
  real(real12),parameter::tolerance=epsilon(pi)
  real(real12)::testx,testy,testz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  testx=kgridFine(1,1,1)%kx
  testy=kgridFine(1,1,1)%ky
  testz=kgridFine(1,1,1)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridFine(fine_points,1,1)%kx
  testy=kgridFine(fine_points,1,1)%ky
  testz=kgridFine(fine_points,1,1)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridFine(1,fine_points,1)%kx
  testy=kgridFine(1,fine_points,1)%ky
  testz=kgridFine(1,fine_points,1)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridFine(1,1,fine_points)%kx
  testy=kgridFine(1,1,fine_points)%ky
  testz=kgridFine(1,1,fine_points)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridFine(fine_points,fine_points,1)%kx
  testy=kgridFine(fine_points,fine_points,1)%ky
  testz=kgridFine(fine_points,fine_points,1)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridFine(fine_points,1,fine_points)%kx
  testy=kgridFine(fine_points,1,fine_points)%ky
  testz=kgridFine(fine_points,1,fine_points)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridFine(1,fine_points,fine_points)%kx
  testy=kgridFine(1,fine_points,fine_points)%ky
  testz=kgridFine(1,fine_points,fine_points)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridFine(fine_points,fine_points,fine_points)%kx
  testy=kgridFine(fine_points,fine_points,fine_points)%ky
  testz=kgridFine(fine_points,fine_points,fine_points)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,last,tolerance)

  
end test
  


test coarse_grid_values_corners
  real(real12),parameter::first=0.0_real12
  real(real12),parameter::last=pi
  real(real12),parameter::tolerance=epsilon(pi)
  real(real12)::testx,testy,testz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  testx=kgridCoarse(1,1,1)%kx
  testy=kgridCoarse(1,1,1)%ky
  testz=kgridCoarse(1,1,1)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridCoarse(ncoarse_cells,1,1)%kx
  testy=kgridCoarse(ncoarse_cells,1,1)%ky
  testz=kgridCoarse(ncoarse_cells,1,1)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridCoarse(1,ncoarse_cells,1)%kx
  testy=kgridCoarse(1,ncoarse_cells,1)%ky
  testz=kgridCoarse(1,ncoarse_cells,1)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridCoarse(1,1,ncoarse_cells)%kx
  testy=kgridCoarse(1,1,ncoarse_cells)%ky
  testz=kgridCoarse(1,1,ncoarse_cells)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kx
  testy=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%ky
  testz=kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,first,tolerance)

  testx=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kx
  testy=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%ky
  testz=kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,first,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kx
  testy=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%ky
  testz=kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kz
  assert_equal_within(testx,first,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,last,tolerance)

  testx=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kx
  testy=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%ky
  testz=kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kz
  assert_equal_within(testx,last,tolerance)
  assert_equal_within(testy,last,tolerance)
  assert_equal_within(testz,last,tolerance)
  
end test

test assigncell_maps
   integer,parameter::nfine=27
   integer,parameter::maxcells=ncoarse_cells**3
   integer,allocatable::cellCount(:,:,:)
   integer:: icell,testno
   logical:: npwrong

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

   !test the number of points mapped to each cell is correct
   allocate(cellcount(fine_points,fine_points,fine_points))
   !loop over cells
   npwrong=.false.
   do icell=1,maxcells
      cellcount=0
      where (kgridFine%coarsemap==icell) cellcount=1
      testno=sum(cellcount)
      if (testno.ne.nfine) then
      	 npwrong=.true.
	 exit
      endif
   enddo
   assert_false(npwrong)
   deallocate(cellcount)
   
end test

test assigncell_specific_fine_points
   integer::testcell

  call initgrid(settings,kgridFine,kgridCoarse,ierr)
   
   !test specific cells
   testcell=kgridfine(5,5,4)%coarsemap
   assert_equal(testcell,8)
   testcell=kgridfine(1,1,1)%coarsemap
   assert_equal(testcell,1)
   testcell=kgridfine(1,1,6)%coarsemap
   assert_equal(testcell,5)
   testcell=kgridfine(1,6,6)%coarsemap
   assert_equal(testcell,7)
   testcell=kgridfine(6,6,6)%coarsemap
   assert_equal(testcell,8)
   
end test
    
test assign_coarse_labels
   integer::ix,iy,iz,itest
   logical::problem

   call initgrid(settings,kgridFine,kgridCoarse,ierr)

   problem=.false.
   do ix=1,ncoarse_cells
      if (problem) exit
      do iy=1,ncoarse_cells
        if (problem) exit
        do iz=1,ncoarse_cells
           itest=ix+(iy-1)*ncoarse_cells+(iz-1)*ncoarse_cells*ncoarse_cells
	   if (itest.ne.kgridcoarse(ix,iy,iz)%label) then
	      problem=.true.
	      exit
	   endif
	enddo
      enddo
   enddo
   assert_false(problem)
   
   
end test

test fine_points_reals_are_numbers
  logical::problem
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kx)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%ky)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
	   
  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(kgridfine(ix,iy,iz)%norm)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   if (isnan(real(kgridfine(ix,iy,iz)%omega2))) problem=.true.
	   if (isnan(aimag(kgridfine(ix,iy,iz)%omega2))) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test fine_grid_kx_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kx
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kx
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test
  
test fine_grid_ky_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%ky
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%ky
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test fine_grid_kz_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kz
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%kz
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test fine_grid_knorm_within_range
  real(real12),parameter::upper=sqrt(27.0_real12*pi*pi/4.0_real12)
  real(real12),parameter::lower=0.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%norm
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,fine_points
     if (problem) exit
     do iy=1,fine_points
     	if (problem) exit
	do iz=1,fine_points
	   test=kgridfine(ix,iy,iz)%norm
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

!!!!!!!!!!!!!!!!!!!
test Coarse_points_reals_are_numbers
  logical::problem
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kx)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%ky)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
	   
  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%kz)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(kgridCoarse(ix,iy,iz)%norm)) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   if (isnan(real(kgridCoarse(ix,iy,iz)%omega2))) problem=.true.
	   if (isnan(aimag(kgridCoarse(ix,iy,iz)%omega2))) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test Coarse_grid_kx_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kx
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kx
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test
  
test Coarse_grid_ky_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%ky
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%ky
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test Coarse_grid_kz_within_range
  real(real12),parameter::upper=3.0_real12*pi/2.0_real12
  real(real12),parameter::lower=-pi/2.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kz
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%kz
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

test Coarse_grid_knorm_within_range
  real(real12),parameter::upper=sqrt(27.0_real12*pi*pi/4.0_real12)
  real(real12),parameter::lower=0.0_real12
  logical::problem
  real(real12)::test
  integer::ix,iy,iz

  call initgrid(settings,kgridFine,kgridCoarse,ierr)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%norm
	   if (test.lt.lower) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)

  problem=.false.
  do ix=1,ncoarse_cells
     if (problem) exit
     do iy=1,ncoarse_cells
     	if (problem) exit
	do iz=1,ncoarse_cells
	   test=kgridCoarse(ix,iy,iz)%norm
	   if (test.ge.upper) problem=.true.
	   if (problem) exit
	enddo
     enddo
  enddo
  assert_false(problem)
  
end test

end test_suite
  
