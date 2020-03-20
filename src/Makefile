obj =   constants.f08 \
	definedtypes.f08 \
	greensroutines.f08 \
	gf_fourier.f08 \
	readin.f08 \
	dispersions.f08 \
	initialisation.f08


compiler = gfortran-9

opt = -fcheck=all -Wall
trap = -ffpe-trap=invalid,overflow,zero
prec = -ffpe-trap=precision
inc = -I/usr/include/
lib = -lfftw3q -lquadmath -lm 

xtest:

	$(compiler) $(opt) -O3 $(inc) $(obj) $(lib)  TMDCmain.f08 -o ../../C-Rhediadau-Anderson/xtest

debug:

	$(compiler) $(opt) $(trap) $(prec) -O3 -g  $(inc) $(obj) $(lib) TMDCmain.f08 -o ../../C-Rhediadau-Anderson/debug

clean:
	rm *mod
