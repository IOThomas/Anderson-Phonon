obj =   constants.f90 \
	definedTypes.f90


compiler = gfortran-8

xtest:
	$(compiler) -O3  $(obj) -o xtest

clean:
	rm *mod
