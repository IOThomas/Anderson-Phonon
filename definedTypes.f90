module definedTypes
  ! contains the defined types for the code
  use constants
  implicit none

  type realVector
     ! defines a real vector type
     real(real12):: Comp(3), Norm
  end type RealVector

  type complexVector
     ! defines complex vector type
     complex(kind=real12)::Comp(3),Norm
  end type complexVector

  type atomicBasis
     ! defines atomic basis
     integer::nAtom
     type(realVector)::unitClength ! length of unit cell
     type(realVector),allocatable,dimension(:)::rBasis,kBasis
  end type atomicBasis

  type cellBasis
     ! defines cells
     integer::nCell!number of cells along one side
     integer::nCelltot
     type(realVector)::clusterLength
     type(realVector),allocatable,dimension(:,:,:)::rBasis,kBasis
     !note that kBasis denotes the bits that are in the *centre*
     !of the cell as per Fig 3 of PRB
  end type cellBasis

end module definedTypes
