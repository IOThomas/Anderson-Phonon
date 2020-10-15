PFUNIT_DIR = /home/iorwerth/pFUnit/build/installed
include $(PFUNIT_DIR)/PFUNIT-4.1/include/PFUNIT.mk
FC1 = gfortran-9

ifeq (nagfor,$(findstring nagfor,$(FC)))
  FFLAGS += -fpp
endif
SRCDIR = ../../src

all: make_helper test_helpers test_dispersions
.PHONY: all

make_helper:
	$(MAKE) -C helper_modules all

FFLAGS += $(PFUNIT_EXTRA_FFLAGS)
FFLAGS += -I$(SRCDIR) -I./helper_modules

%.o: helper_modules/%.f08   
	$(FC1) -c $(FFLAGS)  $<

%.o: ${SRCDIR}/%.f08   
	$(FC1) -c $(FFLAGS)  $<

%.o: %.F90 
	$(FC1) -c $(FFLAGS)  $<


test_helpers_TESTS := test_helpers.pf
test_helpers_OTHER_SRCS := greensroutines_helper greensroutines constants
$(eval $(call make_pfunit_test,test_helpers))

test_dispersions_TESTS := test_dispersions.pf 
test_dispersions_OTHER_SRCS := dispersions constants 
$(eval $(call make_pfunit_test,test_dispersions))

.PHONY: clean
clean:
	$(RM) *.o *.mod *.a *.inc
	$(RM) test_dispersions.F90 test_helpers.F90
	$(MAKE) -C helper_modules clean

.PHONY: cleantests
cleantests:
	$(RM) test_dispersions test_helpers

###########################################################################
.PHONY: runtests
runtests:
	./test_helpers
	./test_dispersions
	
