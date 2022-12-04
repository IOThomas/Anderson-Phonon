#call make $target MODE=debug to turn on gdb flag in compiler
PFUNIT_DIR = /home/iori/lib/pFUnit/build/installed
include $(PFUNIT_DIR)/PFUNIT-4.2/include/PFUNIT.mk
FC1 = gfortran-9
MODE = production

SRCDIR = ../src

all: make_helper \
	test_helpers \
	test_greensroutines \
	test_dispersions \
	test_initialisation \
	test_fastFourier \
	test_config_gen \
	test_config_avg 
.PHONY: all

make_helper:
	$(MAKE) -C helper_modules all

ifeq ($(MODE), debug)
FFLAGS += -g
endif	
FFLAGS += $(PFUNIT_EXTRA_FFLAGS)
FFLAGS += -I$(SRCDIR) -I./helper_modules -I/usr/include/ 

#%.o: %.mod

%.o: helper_modules/%.f08   
	$(FC1) -c $(FFLAGS)  $<

%.o: ${SRCDIR}/%.f08   
	$(FC1) -c $(FFLAGS)  $<

%.o: %.F90 
	$(FC1) -c $(FFLAGS)  $<


test_helpers_TESTS := test_helpers.pf
test_helpers_OTHER_SRCS := greensroutines_helper greensroutines constants
$(eval $(call make_pfunit_test,test_helpers))

test_greensroutines_TESTS := test_greensroutines.pf
test_greensroutines_OTHER_SRCS := greensroutines_helper greensroutines definedtypes constants
$(eval $(call make_pfunit_test,test_greensroutines))

test_dispersions_TESTS := test_dispersions.pf 
test_dispersions_OTHER_SRCS := dispersions constants 
$(eval $(call make_pfunit_test,test_dispersions))

test_initialisation_TESTS := test_initHybrid.pf test_initDzero.pf test_initGrid.pf
test_initialisation_OTHER_SRCS := initialisation dispersions greensroutines definedtypes constants
$(eval $(call make_pfunit_test,test_initialisation))

test_fastFourier_TESTS := test_one_dimensional_FT.pf test_gfFourier.pf test_gf_transforms.pf 
test_fastFourier_OTHER_SRCS := one_dimensional_FT gf_transforms gf_fourier greensroutines_helper greensroutines constants
test_fastFourier_OTHER_LIBRARIES := -lfftw3q -lquadmath -lm
$(eval $(call make_pfunit_test,test_fastFourier))

test_kramers_kronig_TESTS := test_kramers_kronig.pf 
test_kramers_kronig_OTHER_SRCS := kramers_kronig  greensroutines definedtypes constants
$(eval $(call make_pfunit_test,test_kramers_kronig))

test_config_gen_TESTS := test_random_config.pf 
test_config_gen_OTHER_SRCS := random_config  constants
$(eval $(call make_pfunit_test,test_config_gen))

test_config_avg_TESTS := test_config_avg.pf 
test_config_avg_OTHER_SRCS := config_avg greensroutines constants
$(eval $(call make_pfunit_test,test_config_avg))

.PHONY: clean
clean:
	$(RM) *.o *.mod *.smod *.a *.inc
	$(RM) *.F90
	$(MAKE) -C helper_modules clean

.PHONY: cleantests
cleantests:
	$(RM) test_dispersions test_greensroutines test_helpers test_initialisation test_fastFourier test_config_gen

###########################################################################
.PHONY: runtests
runtests:
	./test_helpers
	./test_greensroutines
	./test_dispersions
	./test_initialisation
	./test_fastFourier
	
