obj =   constants.f90 \
	definedTypes.f90 \
	readin.f90 \
	dispersions.f90 \
	initialisation.f90


compiler = gfortran

xtest:

	$(compiler) -O3  $(obj) TMDCtest.f90 -o ../C-Rhediadau-Anderson/xtest

clean:
	rm *mod
