PFUNIT_DIR = /home/iorwerth/pFUnit/build/installed
include $(PFUNIT_DIR)/PFUNIT-4.1/include/PFUNIT.mk
FC1 = gfortran-9

ifeq (nagfor,$(findstring nagfor,$(FC)))
  FFLAGS += -fpp
endif
SRCDIR = ../../src

all: my_tests

FFLAGS += $(PFUNIT_EXTRA_FFLAGS)
FFLAGS += -I../../src 

%.o: ${SRCDIR}/%.f08   #not entirely sure how this only manages to compile the required files from the src directory but oh well...
	$(FC1) -c $(FFLAGS)  $< 

%.o : %.F90 
	$(FC1) -c $(FFLAGS)  $< 

my_tests_TESTS := test_dispersions.pf #put test files for suite here
my_tests_OTHER_SRCS := dispersions constants #put object file dependencies here
$(eval $(call make_pfunit_test,my_tests))


clean:
	$(RM) *.o *.mod *.a *.inc
	$(RM) test_dispersions.F90