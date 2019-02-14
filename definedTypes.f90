module definedTypes
  ! contains the defined types for the code
  use constants
  implicit none

  type finegrid
     real(real12)::kx,ky,kz,norm !defined as centre of cell
     complex(real12)::omega2
     integer::coarseMap !which coarse point to map to
  end type finegrid

  type coarsegrid
     real(real12)::kx,ky,kz,norm !defined as centre of cell
     complex(real12)::omega2
  end type coarsegrid

  
end module definedTypes
