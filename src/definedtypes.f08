module definedtypes
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!# Contains the derived types used in the program.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  use constants, only: real12, zero, cmplx_zero
  implicit none
  private
  public basis, kappagrid, settingparam, storedparam

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type basis
     !# Atomic basis
     integer                   :: natom
     !# number of atoms
     real(real12), allocatable :: rx(:)
     !# *x* position of atom
     real(real12), allocatable :: ry(:)
     !# *y* position of atom
     real(real12), allocatable :: rz(:)
     !# *z* position of atom
  end type basis
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type kappagrid
     !# momentum grid (points defined as being at **centre** of cell)
     real(real12)    :: kx
     !# *x* momentum
     real(real12)    :: ky
     !# *y* momentum
     real(real12)    :: kz
     !# *z* momentum
     real(real12)    :: norm
     !# norm of momentum
     complex(real12) :: omega2
     !# frequency squared
     integer         :: map
     !# associated coarse grid point
  end type kappagrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type settingparam
     !# settings used in calculation
     integer         :: ncell(3)
     !# number of coarse grid points/cells per direction, convention (x, y, z)
     integer         :: nomega
     !# number of omega values
     integer         :: nfpoints(3)
     !# number of fine grid points per direction, convention (x, y, z)
     complex(real12) :: omegaMax
     !# maximum frequency considered
     complex(real12) :: omegaMin=cmplx_zero
     !# minumum frequency considered (default 0+i0)
  end type settingparam
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  type storedparam
     !# stored, calculated parameters
     complex(real12) :: omega_diff
     !# frequency point spacing
  end type storedparam
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module definedTypes
