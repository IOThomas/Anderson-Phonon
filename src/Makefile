obj =   constants.f08 \
	definedtypes.f08 \
	readin.f08 \
	dispersions.f08 \
	initialisation.f08


compiler = gfortran-8

opt = -fcheck=all

xtest:

	$(compiler) $(opt) -O3  $(obj) TMDCmain.f08 -o ../../C-Rhediadau-Anderson/xtest 

clean:
	rm *mod
