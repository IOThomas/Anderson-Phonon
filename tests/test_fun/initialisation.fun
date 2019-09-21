test_suite initialisation

!global variables here
integer,  parameter :: fine_points=6   !changing this will break tests
integer,  parameter :: ncoarse_cells=2 !changing this will break tests
type(settingparam)  :: settings
type(finegrid),allocatable     :: kgridFine(:,:,:)
type(coarsegrid),allocatable   :: kgridCoarse(:,:,:)


setup              
  !code that should be run before each test
  settings%nfpoints=fine_points
  settings%ncell=ncoarse_cells
  call initgrid(settings,kgridFine,kgridCoarse)
end setup

teardown
  !code runs after each test
  integer::stat

  deallocate(kgridFine,kgridCoarse)
end teardown

test fine_points_stored_correctly
  assert_equal(settings%nfpoints(1),fine_points)
  assert_equal(settings%nfpoints(2),fine_points)
  assert_equal(settings%nfpoints(3),fine_points)
end test

test coarse_cell_no_stored_correctly
  assert_equal(settings%ncell(1),ncoarse_cells)
  assert_equal(settings%ncell(2),ncoarse_cells)
  assert_equal(settings%ncell(3),ncoarse_cells)
end test

test fine_grid_array_size
  integer:: ix,iy,iz

  ix=size(kgridFine,1)
  iy=size(kgridFine,2)
  iz=size(kgridFine,3)

  assert_equal(ix,fine_points)
  assert_equal(iy,fine_points)
  assert_equal(iz,fine_points)

end test

test coarse_grid_array_size
  integer:: ix,iy,iz

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

 
  assert_equal_with(kgridFine(1,1,1)%kx,first,tolerance)
  assert_equal_with(kgridFine(1,1,1)%ky,first,tolerance)
  assert_equal_with(kgridFine(1,1,1)%kz,first,tolerance)

  assert_equal_with(kgridFine(fine_points,1,1)%kx,last,tolerance)
  assert_equal_with(kgridFine(fine_points,1,1)%ky,first,tolerance)
  assert_equal_with(kgridFine(fine_points,1,1)%kz,first,tolerance)

  assert_equal_with(kgridFine(1,fine_points,1)%kx,first,tolerance)
  assert_equal_with(kgridFine(1,fine_points,1)%ky,last,tolerance)
  assert_equal_with(kgridFine(1,fine_points,1)%kz,first,tolerance)

  assert_equal_with(kgridFine(1,1,fine_points)%kx,first,tolerance)
  assert_equal_with(kgridFine(1,1,fine_points)%ky,first,tolerance)
  assert_equal_with(kgridFine(1,1,fine_points)%kz,last,tolerance)

  assert_equal_with(kgridFine(fine_points,fine_points,1)%kx,last,tolerance)
  assert_equal_with(kgridFine(fine_points,fine_points,1)%ky,last,tolerance)
  assert_equal_with(kgridFine(fine_points,fine_points,1)%kz,first,tolerance)

  assert_equal_with(kgridFine(1,fine_points,fine_points)%kx,first,tolerance)
  assert_equal_with(kgridFine(1,fine_points,fine_points)%ky,last,tolerance)
  assert_equal_with(kgridFine(1,fine_points,fine_points)%kz,last,tolerance)

  assert_equal_with(kgridFine(fine_points,1,fine_points)%kx,last,tolerance)
  assert_equal_with(kgridFine(fine_points,1,fine_points)%ky,first,tolerance)
  assert_equal_with(kgridFine(fine_points,1,fine_points)%kz,last,tolerance)
  
  assert_equal_with(kgridFine(fine_points,fine_points,fine_points)%kx,&
	last,tolerance)
  assert_equal_with(kgridFine(fine_points,fine_points,fine_points)%ky,&
	last,tolerance)
  assert_equal_with(kgridFine(fine_points,fine_points,fine_points)%kz,&
	last,tolerance)

 end test
  


test coarse_grid_values_corners
  real(real12),parameter::first=0.0_real12
  real(real12),parameter::last=pi
  real(real12),parameter::tolerance=epsilon(pi)

 
  assert_equal_with(kgridCoarse(1,1,1)%kx,first,tolerance)
  assert_equal_with(kgridCoarse(1,1,1)%ky,first,tolerance)
  assert_equal_with(kgridCoarse(1,1,1)%kz,first,tolerance)

  assert_equal_with(kgridCoarse(ncoarse_cells,1,1)%kx,last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,1,1)%ky,first,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,1,1)%kz,first,tolerance)

  assert_equal_with(kgridCoarse(1,ncoarse_cells,1)%kx,first,tolerance)
  assert_equal_with(kgridCoarse(1,ncoarse_cells,1)%ky,last,tolerance)
  assert_equal_with(kgridCoarse(1,ncoarse_cells,1)%kz,first,tolerance)

  assert_equal_with(kgridCoarse(1,1,ncoarse_cells)%kx,first,tolerance)
  assert_equal_with(kgridCoarse(1,1,ncoarse_cells)%ky,first,tolerance)
  assert_equal_with(kgridCoarse(1,1,ncoarse_cells)%kz,last,tolerance)

  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kx,last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,1)%ky,last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,1)%kz,first,tolerance)

  assert_equal_with(kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kx,first,tolerance)
  assert_equal_with(kgridCoarse(1,ncoarse_cells,ncoarse_cells)%ky,last,tolerance)
  assert_equal_with(kgridCoarse(1,ncoarse_cells,ncoarse_cells)%kz,last,tolerance)

  assert_equal_with(kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kx,last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,1,ncoarse_cells)%ky,first,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,1,ncoarse_cells)%kz,last,tolerance)
  
  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kx,&
	last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%ky,&
	last,tolerance)
  assert_equal_with(kgridCoarse(ncoarse_cells,ncoarse_cells,ncoarse_cells)%kz,&
	last,tolerance)

 end test

 test_assigncell_fine_points_count
   integer,parameter::nfine=27
   integer,parameter::maxcells=ncoarse_cells**3
   integer,allocatable::cellCount
   integer:: icell,testno

   !test the number of points mapped to each cell is correct
   allocate(cellcount(fine_points,fine_points,fine_points))
   !loop over cells
   do icell=1,maxcells
      cellcount=0
      where (kfinegrid==icell) cellcount=1
      testno=sum(cellcount)
      assert_equal(testno,nfine)
   enddo
   deallocate(cellcount)
 end test

 test_assigncell_specific_fine_points
   !test specific cells
   assert_equal(kgridfine(5,5,4)%coarsemap,8)
   assert_equal(kgridfine(1,1,1)%coarsemap,1)
   assert_equal(kgridfine(1,1,6)%coarsemap,5)
   assert_equal(kgridfine(1,6,6)%coarsemap,7)
   assert_equal(kgridfine(6,6,6)%coarsemap,8)

 end test
    
 test_assign_coarse_labels
   integer::ix,iy,iz,itest

   !loop over cells
   do ix=1,ncoarse_cells
      do iy=1,ncoarse_cells
       	 do iz=1,ncoarse_cells
	    itest=ix+(iy-1)*ncoarse_cells+(iz-1)*ncoarse_cells*ncoarse_cells
	    assert_equal(itest,kgridcoarse(ix,iy,iz)%label)
 	 enddo
      enddo
   enddo

 end test

end test_suitea
