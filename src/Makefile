obj =   constants.f08 \
	definedTypes.f08 \
	readin.f08 \
	dispersions.f08 \
	initialisation.f08


compiler = gfortran-8

xtest:

	$(compiler) -O3  $(obj) TMDCtest.f08 -o ../../C-Rhediadau-Anderson/xtest

clean:
	rm *mod
