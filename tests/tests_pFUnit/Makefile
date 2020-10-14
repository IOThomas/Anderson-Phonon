PFUNIT_DIR = /home/iorwerth/pFUnit/build/installed
include $(PFUNIT_DIR)/PFUNIT-4.1/include/PFUNIT.mk
FC1 = gfortran-9

ifeq (nagfor,$(findstring nagfor,$(FC)))
  FFLAGS += -fpp
endif
SRCDIR = ../../src

all: test_helpers test_dispersions

FFLAGS += $(PFUNIT_EXTRA_FFLAGS)
FFLAGS += -I../../src -I./helper_modules

#%.o: ./helper_modules/%.f08
#	$(FC1) -c $(FFLAGS)  $<

%.o: ./helper_modules/%.f08 ${SRCDIR}/%.f08   
	$(FC1) -c $(FFLAGS)  $< 

%.o : %.F90 
	$(FC1) -c $(FFLAGS)  $< 

test_helpers_TESTS := test_helpers.pf
test_helpers_OTHER_SRCS := greensroutines_helper greensroutines constants
$(eval $(call make_pfunit_test,test_helpers))

test_dispersions_TESTS := test_dispersions.pf #put test files for suite here
test_dispersions_OTHER_SRCS := dispersions constants #put object file dependencies here
$(eval $(call make_pfunit_test,test_dispersions))


clean:
	$(RM) *.o *.mod *.a *.inc
	$(RM) test_dispersions.F90