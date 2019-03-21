module initialisation
  use constants
  use dispersions
  use definedTypes
  implicit none

contains

  subroutine initGrid(settings,kgridFine,kgridCoarse)
    type(settingparam),intent(in)::settings
    type(finegrid),allocatable,intent(inout)::kgridFine(:,:,:)
    type(coarsegrid),allocatable,intent(inout)::kgridCoarse(:,:,:)

    !routine variables
    integer::itemp(3),ix,iy,iz,iNcell,iNpoint
    real(real12)::kx,ky,kz,length(3),lowLim(3),upperLim(3)
    complex(real12),allocatable::tempArray(:,:,:)


    !allocate fine grid
    itemp=settings%nfpoints
    length=2.0*pi/real(itemp-1,real12)
    allocate(kgridFine(itemp(1),itemp(2),itemp(3)))
    do ix=1,itemp(1)
       kx=real(ix-1)*length(1)-pi+0.5d0*length(1)
       do iy=1,itemp(2)
          ky=real(iy-1)*length(2)-pi+0.5d0*length(2)
          do iz=1,itemp(3)
             ky=real(iz-1)*length(3)-pi+0.5d0*length(3)
             kgridFine(ix,iy,iz)%kx=kx
             kgridFine(ix,iy,iz)%ky=ky
             kgridFine(ix,iy,iz)%kz=kz
             kgridFine(ix,iy,iz)%norm=sqrt(kx*kx+ky*ky+kz*kz)
             kgridFine(ix,iy,iz)%omega2=fineDispersion(kx,ky,kz)
          enddo
       enddo
    enddo
    
    !allocate coarsegrid
    itemp=settings%ncell
    allocate(kgridCoarse(itemp(1),itemp(2),itemp(3)))
    length=2.0*pi/real(itemp-1,real12)
    allocate(kgridFine(itemp(1),itemp(2),itemp(3)))
    do ix=1,itemp(1)
       kx=real(ix-1)*length(1)-pi+0.5d0*length(1)
       do iy=1,itemp(2)
          ky=real(iy-1)*length(2)-pi+0.5d0*length(2)
          do iz=1,itemp(3)
             ky=real(iz-1)*length(3)-pi+0.5d0*length(3)
             kgridFine(ix,iy,iz)%kx=kx
             kgridFine(ix,iy,iz)%ky=ky
             kgridFine(ix,iy,iz)%kz=kz
             kgridFine(ix,iy,iz)%norm=sqrt(kx*kx+ky*ky+kz*kz)
             kgridFine(ix,iy,iz)%omega2=fineDispersion(kx,ky,kz)
          enddo
       enddo
    enddo
    
    !allocate coarsegrid
    itemp=settings%ncell
    allocate(kgridCoarse(itemp(1),itemp(2),itemp(3)))
    length=2.0*pi/real(itemp-1,real12)
    do ix=1,itemp(1)
       kx=real(ix-1)*length(1)-pi+0.5d0*length(1)
       do iy=1,itemp(2)
          ky=real(iy-1)*length(2)-pi+0.5d0*length(2)
          do iz=1,itemp(3)
             kz=real(iz-1)*length(3)-pi+0.5d0*length(3)
             kgridCoarse(ix,iy,iz)%kx=kx
             kgridCoarse(ix,iy,iz)%ky=ky
             kgridCoarse(ix,iy,iz)%kz=kz
             kgridCoarse(ix,iy,iz)%norm=sqrt(kx*kx+ky*ky+kz*kz)
             kgridCoarse(ix,iy,iz)%label=ix+(iy-1)*itemp(1)&
                  +(iz-1)*itemp(1)*itemp(2)
          enddo
       enddo
    enddo

    !allocate the coarse map points
    do ix=1,itemp(1)
       lowLim(1)=real(ix-1)*length(1)-pi
       upperLim(1)=real(ix)*length(1)-pi
       do iy=1,itemp(2)
          lowLim(2)=real(iy-1)*length(2)-pi
          upperLim(2)=real(iy)*length(2)-pi
          do iz=1,itemp(3)
             lowLim(3)=real(iz-1)*length(3)-pi
             upperLim(3)=real(iz)*length(3)-pi
             
             tempArray=0.0d0
             where (( kgridFine%kx >= lowLim(1) ).and.&
                  ( kgridFine%kx < upperLim(1) ).and. &
                  ( kgridFine%ky >= lowLim(2) ).and.&
                  ( kgridFine%ky < upperLim(2) ).and. &
                  ( kgridFine%kz >= lowLim(3) ).and.&
                  ( kgridFine%kz < upperLim(3) ))

                kgridFine%coarsemap=kgridCoarse(ix,iy,iz)%label
                tempArray=kgridFine%omega2
             end where
             kgridCoarse%omega2=sum(tempArray)
          enddo
       enddo
    enddo
    iNcell=settings%ncell(1)*settings%ncell(2)*settings%ncell(2)
    iNpoint=settings%nfpoints(1)*settings%nfpoints(2)*settings%nfpoints(2)
    kgridCoarse%omega2=real(iNcell,real12)&
         *kgridCoarse%omega2/real(iNpoint,real12)

    !calculate frequency squared for both grids

    !output grids
    open(unit=10,file='finekgrid.out',status='new')
    write(10) '# fine kpoint grid'
    write(10) 'ix, iy, iz, kx, ky, kz, |k|, omega**2, <map to this coarse grid point>'
    itemp=settings%nfpoints
    do ix=1,itemp(1)
       do iy=1,itemp(2)
          do iz=1,itemp(3)
             write(10,*) ix, iy, iz, kgridFine(ix,iy,iz)%kx, &
                  kgridFine(ix,iy,iz)%ky, kgridFine(ix,iy,iz)%kz,&
                  kgridFine(ix,iy,iz)%norm,&
                  kgridFine(ix,iy,iz)%omega2,kgridFine(ix,iy,iz)%coarsemap
          enddo
       enddo
    enddo
    close(10)
    
    open(unit=10,file='coarsekgrid.out',status='new')
    write(10) '# coarse kpoint grid'
    write(10) 'ix, iy, iz, kx, ky, kz, |k|, omega**2, cell label'
    itemp=settings%ncell
    do ix=1,itemp(1)
       do iy=1,itemp(2)
          do iz=1,itemp(3)
             write(10,*) ix, iy, iz, kgridCoarse(ix,iy,iz)%kx, &
                  kgridCoarse(ix,iy,iz)%ky, kgridCoarse(ix,iy,iz)%kz,&
                  kgridCoarse(ix,iy,iz)%norm,&
                  kgridCoarse(ix,iy,iz)%omega2,kgridCoarse(ix,iy,iz)%label
          enddo
       enddo
    enddo
    close(10)

    deallocate(tempArray)
  end subroutine initGrid

  subroutine initGF()

    !generate DO for fine grid points

    !map to coarse grid

  end subroutine initGF

  subroutine initHybrid()

    ! generate initial hybridisation

  end subroutine initHybrid

    
end module initialisation
