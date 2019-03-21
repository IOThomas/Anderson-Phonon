program dynamicalLoop

  implicit none

contains

  subroutine DCAphonon(oldHyb,newHyb,dcaGF,typGF,kgridfine,kgridcoarse,&
       atombasis)
    !dummy variables
    type(basis)::atombasis
    type(finegrid)::kgridfine(:,:,:)
    type(coarsegrid)::kgridCoarse(:,:,)
    complex(real12),intent(in)::oldHyb(:,:,:,:)
    complex(real12),intent(out)::newHyb(:,:,:,:)
    complex(real12),intent(out)::dcaGF(:,:,:,;),typGF(:,:,:,:)


    !routine variables


    !calculate cluster excluded GF
    dummy(iKx, iKy, iKz, iomega)= omega(iomega)**2-CGdisp(iKx,iKy,iKz) &
         -oldHyb(iKx,iKy,iKz,iomega)
    clustExGF=invert(dummy) !invert yn isreolydd

    !Get real-space GF -- falle rhowch yn getClusterGF()
    rlspaceExGF=fourier(clustExGF)
    rlspaceExGF=rlspaceExGF/mass

    call getClusterGF()

    !new coarsegrained -- disOUTGF depends on method
    !need to calculate all functions for small k lattice
    !invert
    !map to K lattice
    newClustEXGF=stuff

    !ADOS

    !new hybfunction
    newHyb(iKx,iKy,iKz,iomega)=oldHyb(iKx,iKy,iKz,iomega)&
         +mixParam*(inverse(newClustEXGF)-inverse(disOUTGF))
    
  end subroutine DCAphonon
