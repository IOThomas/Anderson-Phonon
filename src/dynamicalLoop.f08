module dynamicalLoop
  
  use constants
  use definedtypes
  use greensroutines
  use gf_fourier
  implicit none

contains

  subroutine loop(settings, stored, kgrid_coarse, kgrid_fine, GF_mom,&
       & hybridisation, ADOS, TDOS, ierr)
    type(settingparam), intent(in)  :: settings
    type(storedparam), intent(in)   :: stored
    type(kappagrid), intent(in)     :: kgrid_coarse(:, :, :)
    type(kappagrid), intent(in)     :: kgrid_fine(:, :, :)
    type(greensfunc), intent(inout) :: GF_mom(:, :, :)
    type(greensfunc), intent(inout) :: hybridisation(:, :, :)
    real(real12), intent(out)       :: ADOS(:)
    real(real12), intent(out)       :: TDOS(:)
    integer, intent(out)            :: ierr

    !routine variables
    type(greensfunc), allocatable  :: GF_real(:, :)
    type(greensfunc), allocatable  :: GF_typ(:, :)
    type(greensfunc), allocatable  :: new_hybrid(:, :, :)
    integer                        :: ix, iy, iz, iom
    integer                        :: np_coarse, np_fine
    integer                        :: iloop
    integer                        :: ierr1
    logical                        :: converged

    !allocate variables
    np_coarse = settings%ncell(1)*settings%ncell(2)*settings%ncell(3)
    np_fine = settings%nfpoints(1)*settings%nfpoints(2)*settings%nfpoints(3)

    allocate(GF_real(np_coarse, np_coarse))
    do ix = 1, np_coarse
       do iy = 1, np_coarse
          allocate(GF_real(ix, iy)%GF(settings%nomega))
       enddo
    enddo
    
    call allocateGF(new_hybrid, settings%ncell(1), settings%ncell(2),&
         & settings%ncell(3), settings%nomega, ierr1)
    call allocateGF(GF_typ, settings%ncell(1), settings%ncell(2),&
         & settings%ncell(3), settings%nomega, ierr1)

    self_consistent_loop:do iloop = 1, settings%max_loops


       call calculateGF(GF_mom, stored%omega_diff, kgrid_coarse,&
            & hybridisation, ierr1)
       ! error handling

       call momspace_to_realspace(GF_mom, GF_real, ierr1)
       ! error handling

       call disorder(GF_real, GF_typ, ADOS, PDOS, ierr1)
       ! error handling

       call new_hyb(GF_mom, GF_typ, hybridisation, new_hybrid,&
            & settings%mix, ierr1)
       ! error handling

       converged = converge(new_hybrid, hybridisation, &
            & settings%conv_thresh)

       if (converged) exit
       

    end do self_consistent_loop
       
    if (.not.converged) then
       ! not converged error handling
    end if

  end subroutine loop
    

end module 
