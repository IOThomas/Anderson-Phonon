test_suite initialisation

!global variables here
integer,parameter::fine_points=
integer,parameter::ncoarse_cells=
type(settingparam)::settings
type(finegrid)::kgridFine(:,:)
type(coarsegrid)::kgridCoarse(:,:)


setup
  !code that should be run before each test
  settings%nfpoints=fine_points
  settings%ncell=ncoarse_cells
  call initgrid(settings,kgridFine,kgridCoarse)
end setup

teardown
  !code runs after each test
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


end test_suite
