OBJECTS = greensroutines_helper.o

FC1 = gfortran-9
SRCDIR = ../../src
FFLAGS += -I$(SRCDIR) -I.

all: $(OBJECTS)
.PHONY: all

$(OBJECTS): %.o: %.f08
	$(FC1) -c $(FFLAGS)  $^ -o $@

.PHONY: clean
clean:
	$(RM) *.o *.mod
