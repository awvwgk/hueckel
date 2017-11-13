PROG=s-h

###------------------------------------------------------------------------###

OBJS_MOD  = time.o

OBJS_MISC = help.o read.o error.o print.o

OBJS_PROG = hueckel.o

OBJS = $(OBJS_MOD) $(OBJS_MISC) $(OBJS_PROG)

SRCS=$(patsubst %.F, %.o, $(wildcard *.F))
###------------------------------------------------------------------------###
FC=ifort
CC=icc

# compile flags
FCFLAGS = -O -axAVX -qopenmp -g -traceback
CCFLAGS = -O -g -DLINUX -DEBUG
# link flags
FL = ifort -static -fopenmp \
		 -I$(MKLROOT)/include/intel64/lp64 \
		 -I$(MKLROOT)/include
LIBS = $(MKLROOT)/lib/intel64/libmkl_blas95_lp64.a \
			 $(MKLROOT)/lib/intel64/libmkl_lapack95_lp64.a -Wl,--start-group \
			 $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a \
			 $(MKLROOT)/lib/intel64/libmkl_core.a \
			 $(MKLROOT)/lib/intel64/libmkl_intel_thread.a -Wl,--end-group \
			 -lpthread -lm

###------------------------------------------------------------------------###
.PHONY: all
.PHONY: clean

all: $(PROG)

###------------------------------------------------------------------------###
%.o: %.f
	$(FC) $(FCFLAGS) -o $@ -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

###------------------------------------------------------------------------###
$(PROG): $(OBJS)
	$(FL) $^ $(LIBS) -o $@

clean:
	rm *.o 
	rm *.mod 
	rm $(PROG)
