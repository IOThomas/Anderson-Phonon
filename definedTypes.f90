module definedTypes
  ! contains the defined types for the code
  contains constants
  implicit none

  type realVector
     ! defines a real vector type
     real(real12):: xComp, yComp, zComp, Norm
  end type RealVector

  type complexVector
     ! defines complex vector type
     complex(real12)::xComp,yComp,zComp,Norm
  end type complexVector

  type atomicBasis
     ! defines atomic basis
     integer, len::nAtom
     type(realVector)::unitClength ! length of unit cell
     type(realVector),dimension(nAtom)::rBasis,kBasis
  end type atomicBasis

  type cellBasis
     ! defines cells
     integer, len::nCells
     type(realVector)::clusterLength
     type(realVector)::dimension(nCell)::rBasis,kBasis
  end type cellBasis

end module definedTypes
