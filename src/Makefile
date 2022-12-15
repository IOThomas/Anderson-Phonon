obj =   constants.f08 \
	definedtypes.f08 \
	greensroutines.f08 \
	random_config_generator/random_config.f08 \
	random_config_generator/config_avg.f08 \
	gf_fourier.f08 \
	gf_transforms.f08 \
	one_dimensional_FT.f08 \
	readin.f08 \
	dispersions.f08 \
	initialisation.f08


compiler = gfortran-9

opt = -fcheck=all
warn = -Wall
trap = -ffpe-trap=invalid,overflow,zero
prec = -ffpe-trap=precision
inc = -I/usr/include/
lib = -lfftw3q -lquadmath -lm 

xtest:

	$(compiler) $(opt) -O3 $(inc) $(obj) $(lib)  TMDCmain.f08 -o ../../C-Rhediadau-Anderson/xtest

test:

	$(compiler) $(opt) $(trap) $(prec) -c -g  $(inc) $(obj) $(lib) 
	mv *.o ../tests
	mv *.mod ../tests

test-warn:
	$(compiler) $(opt) $(warn) $(trap) $(prec) -c -g  $(inc) $(obj) $(lib) 
	mv *.o ../tests
	mv *.mod ../tests

clean:
	rm *mod
