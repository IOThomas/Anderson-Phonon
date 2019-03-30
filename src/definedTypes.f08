module definedTypes
  ! contains the defined types for the code
  use constants, only: real12
  implicit none
  private
  public basis, finegrid, coarsegrid, settingparam

  type basis
     integer::natom
     real(real12),allocatable::rx(:),ry(:),rz(:)
  end type basis

  type finegrid
     real(real12)::kx,ky,kz,norm !defined as centre of cell
     complex(real12)::omega2
     integer::coarseMap !which coarse point to map to
  end type finegrid

  type coarsegrid
     real(real12)::kx,ky,kz,norm !defined as centre of cell
     complex(real12)::omega2
     integer::label
  end type coarsegrid

  type settingparam
     integer::ncell(3)! number of cells in each direction
     integer::nomega ! number of omega values
     integer::nfpoints(3) !number of fine gridpoints in each direction
     complex(real12),allocatable::omega2diff(:,:,:,:) !omega2 differences
     complex(real12)::omegaMin,omegaMax
     integer,allocatable::pointsmap(:,:,:,:) ! for quick fine to coarse mapping
  end type settingparam

  
end module definedTypes
