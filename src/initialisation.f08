module initialisation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! contains initialisation subroutines for the calculation
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  use constants, only: real12, pi
  use dispersions, only: finedispersion, coarseDispersion
  use definedTypes, only: settingparam,finegrid,coarsegrid
  implicit none
  private
  public initGrid,initD0

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initGrid(settings,kgridFine,kgridCoarse)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initialises coarse and fine momentum grids for the calculation
! Note that the formula for assigning the kpoint values is a little non-obvious
! and only works for *even* values of Ncell and Nfpoints  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    type(settingparam),intent(in)::settings
    type(finegrid),allocatable,intent(inout)::kgridFine(:,:,:)
    type(coarsegrid),allocatable,intent(inout)::kgridCoarse(:,:,:)

    !routine variables
    integer::itemp(3),ix,iy,iz,iNcell,iNpoint
    real(real12)::kx,ky,kz,length(3),lowLim(3),upperLim(3)
    complex(real12),allocatable::tempArray(:,:,:)


    !allocate fine grid
    itemp=settings%nfpoints
    length=2.0_real12*pi/real(itemp,real12)
    allocate(kgridFine(itemp(1),itemp(2),itemp(3)))
    allocate(tempArray(itemp(1),itemp(2),itemp(3)))
    do iz=1,itemp(3)
       kz=(real(iz,real12)-0.5_real12*real(1+(itemp(3)/2),real12))*length(3)
       do iy=1,itemp(2)
          ky=(real(iy,real12)-0.5_real12*real(1+(itemp(2)/2),real12))*length(2)
          do ix=1,itemp(1)
             kx=(real(ix,real12)-0.5_real12*real(1+(itemp(1)/2),real12))&
                  *length(1)
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
    length=2.0_real12*pi/real(itemp,real12)
    do iz=1,itemp(3)
       kz=(real(iz,real12)-0.5_real12*real(1+(itemp(3)/2),real12))*length(3)
       do iy=1,itemp(2)
          ky=(real(iy,real12)-0.5_real12*real(1+(itemp(2)/2),real12))*length(2)
          do ix=1,itemp(1)
             kx=(real(ix,real12)-0.5_real12*real(1+(itemp(1)/2),real12))&
                  *length(1)
             kgridCoarse(ix,iy,iz)%kx=kx
             kgridCoarse(ix,iy,iz)%ky=ky
             kgridCoarse(ix,iy,iz)%kz=kz
             kgridCoarse(ix,iy,iz)%norm=sqrt(kx*kx+ky*ky+kz*kz)
             kgridCoarse(ix,iy,iz)%label=ix+(iy-1)*itemp(1)&
                  +(iz-1)*itemp(1)*itemp(2)
          enddo
       enddo
    enddo

    !allocate the coarse map points and the associated coarse momentum
    iNcell=settings%ncell(1)*settings%ncell(2)*settings%ncell(2)
    iNpoint=settings%nfpoints(1)*settings%nfpoints(2)*settings%nfpoints(2)
    do ix=1,itemp(1)
       do iy=1,itemp(2)
          do iz=1,itemp(3)
             lowLim(1)=kgridCoarse(ix,iy,iz)%kx-0.5_real12*length(1)
             upperLim(1)=kgridCoarse(ix,iy,iz)%kx+0.5_real12*length(1)

             lowLim(2)=kgridCoarse(ix,iy,iz)%ky-0.5_real12*length(2)
             upperLim(2)=kgridCoarse(ix,iy,iz)%ky+0.5_real12*length(2)
          
             lowLim(3)=kgridCoarse(ix,iy,iz)%kz-0.5_real12*length(3)
             upperLim(3)=kgridCoarse(ix,iy,iz)%kz+0.5_real12*length(3)

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
             kgridCoarse(ix,iy,iz)%omega2=&
                  coarseDispersion(tempArray,iNcell,iNpoint)           
          enddo
       enddo
    enddo

    !output grids
    open(unit=10,file='finekgrid.out',status='new')
    write(10,*) '# fine kpoint grid'
    write(10,*) 'ix, iy, iz, kx, ky, kz, |k|, Re(omega**2),  Im(omega**2), <map to this coarse grid point>'
    itemp=settings%nfpoints
    do ix=1,itemp(1)
       do iy=1,itemp(2)
          do iz=1,itemp(3)
             write(10,fmt=800) ix, iy, iz, kgridFine(ix,iy,iz)%kx, &
                  kgridFine(ix,iy,iz)%ky, kgridFine(ix,iy,iz)%kz,&
                  kgridFine(ix,iy,iz)%norm,&
                  kgridFine(ix,iy,iz)%omega2,kgridFine(ix,iy,iz)%coarsemap
          enddo
       enddo
    enddo
    close(10)
    
    open(unit=10,file='coarsekgrid.out',status='new')
    write(10,*) '# coarse kpoint grid'
    write(10,*) 'ix, iy, iz, kx, ky, kz, |k|, Re(omega**2), Im(omega**2), cell label'
    itemp=settings%ncell
    do ix=1,itemp(1)
       do iy=1,itemp(2)
          do iz=1,itemp(3)
             write(10,fmt=800) ix, iy, iz, kgridCoarse(ix,iy,iz)%kx, &
                  kgridCoarse(ix,iy,iz)%ky, kgridCoarse(ix,iy,iz)%kz,&
                  kgridCoarse(ix,iy,iz)%norm,&
                  kgridCoarse(ix,iy,iz)%omega2,kgridCoarse(ix,iy,iz)%label
          enddo
       enddo
    enddo
    close(10)

    deallocate(tempArray)
!!!!!!!!!!!!!!!!!
! FORMAT BLOCKS !
!!!!!!!!!!!!!!!!!
800 format(3i5,4es25.15,2es25.15,i5)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  end subroutine initGrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine initD0(settings,kgridFine,DnDisO)
    type(settingparam),intent(inout)::settings
    type(finegrid),allocatable,intent(inout)::kgridFine(:,:,:)
    complex(real12),intent(inout)::DnDisO(:,:,:,:)

    ! routine variables
    integer::i,j,k,nmax
    complex(real12)::omega_diff,tempArray

    !generate omega2 differences
    allocate(settings%omega2diff(nfpoints(1),nfpoints(2),nfpoints(3),&
         settings%nomega))
    omega_diff=(settings%omegaMax-settings%omegaMin)/&
         (real(settings%nomega-1,real12))

    do i=1,settings%nomega
       settings%omega2diff(1:nfpoints(1),1:nfpoints(2),1:nfpoints(3),i)=&
            ((i-1)*omega_diff)**2&
            -(kgridFine(1:nfpoints(1),1:nfpoints(2),1:nfpoints(3))**2
    enddo
    allocate(pointsmap,mold=kgridFine)
    pointsmap=kgridFine%coarseMap
    
    !generate DO for fine grid points
    allocate(tempArray,mold=kgridFine)
    allocate(DnDisO(settings%ncell(1),settings%ncell(2),settings%ncell(3),&
         settings%nomega)
    
    !map to coarse grid
    tempArray=0.0_real12
    nmax=settings%ncell(1)*settings%ncell(2)*settings%ncell(3)
    nfmax=settings%nfpoints(1)*settings%nfpoints(2)*settings%nfpoints(3)
    do i=nmax
       where(kpgridFine%coarseMap=i)
          ! will need to rework for matrix case?
          tempArray=1.0_real12/settings%omega2diff
       end where
       DnDisO=
  end subroutine initD0

  subroutine initHybrid()

    ! generate initial hybridisation

  end subroutine initHybrid

    
end module initialisation
