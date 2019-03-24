module readsettings

  use constants
  use definedTypes

  implicit none

contains

  subroutine readin(atombasis,settings)
    type(basis),intent(inout)::atombasis
    type(settingparam),intent(inout)::settings

    !am nawr, s'dim ffeil osodiadau i'w ddarllen, jyst y osodiadau syml yma.

    atomBasis%natom=1 !un atom y cell, ni'n gweithio â gell sgwâr
    allocate(atomBasis%rx(atomBasis%natom),atomBasis%ry(atomBasis%natom))
    allocate(atomBasis%rz(atomBasis%natom))

    atomBasis%rx=0.0d0 !atom yn byw ar gornel y cell
    atomBasis%ry=0.0d0
    atomBasis%rz=0.0d0

    settings%ncell=2 !rhoi 4 cell ym mhob cyfeiriad -- gwiriwch bod hyn
                     !yn gyson a'r gymesuredd

    settings%nomega=200
    settings%nfpoints=4 !rhoi 10 pwynt ym mhob cyfeiriad...

  end subroutine readin
end module readsettings
