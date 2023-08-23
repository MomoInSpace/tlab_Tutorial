.SUFFIXES:
FC=gfortran
COMPILE.f03 = $(FC) $(FCFLAGS) $(TARGET_ARCH) -c

SOURCES= all_kinds 0a_evolution_equation 0b_linear_adv_equation 

objects = 0a_evolution_equation 0b_linear_adv_equation 
all: $(objects)

$(objects): %: %.f03
	$(FC) $(FCFLAGS) -o $@ $<
.PHONY: clean
# main: $(subst .f90,.o,$(SOURCES))
# 	$(FC) -o $@ $+

# .PHONY: clean
clean:
	-rm -f *.o *.mod *.smod main
