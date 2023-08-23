.SUFFIXES:
FC=gfortran
COMPILE.f03 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

SOURCES= tlab_constants.f90 tlab_arrays.f90 array_calc.f90

# objects = 0a_evolution_equation 0b_linear_adv_equation 
# all: $(objects)

# $(objects): %: %.f03
# 	$(FC) $(FCFLAGS) -o $@ $<
# .PHONY: clean
array_calc: $(subst .f03,.o,$(SOURCES))
	$(FC) -o $@ $+

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
