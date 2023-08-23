.SUFFIXES:
FC=gfortran
COMPILE.f03 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

SOURCES1= tlab_constants.f90 matrix_output.f90  array_calc.f90
SOURCES2= tlab_constants.f90 tlab_arrays.f90 array_calc_tlab.f90

# objects = 0a_evolution_equation 0b_linear_adv_equation 
# all: $(objects)

# $(objects): %: %.f03
# 	$(FC) $(FCFLAGS) -o $@ $<
# .PHONY: clean
# array_calc_tlab: $(subst .f03,.o,$(SOURCES2))
# 	$(FC) -o $@ $+

# .PHONY: clean
array_calc: $(subst .f03,.o,$(SOURCES1))
	$(FC) -o $@ $+

.PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
