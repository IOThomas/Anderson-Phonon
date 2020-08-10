PFUNIT_DIR = /home/iorwerth/pFUnit/build/installed
include $(PFUNIT_DIR)/PFUNIT-4.1/include/PFUNIT.mk
FFLAGS += $(PFUNIT_EXTRA_FFLAGS)

my_tests_TESTS := test_dispersions.pf
$(eval $(call make_pfunit_test,my_tests))

all:
	$(MAKE) ../../src/dispersions.f08
	$(MAKE) -C ./

%.o : %.F90
	$(FC) -c $(FFLAGS) $<

clean:
	$(MAKE) ../../src clean
	$(MAKE) clean