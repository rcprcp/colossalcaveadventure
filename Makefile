FC = gfortran
CFLAGS = -g -O2

# Compile object files
%.o: %.f90
	$(FC) $(CFLAGS) -c $< -o $@

# Link executable using GCC to ensure static linking of Fortran runtime
advent: compat_mod.o advent_f2023_full.o io_mod.o data_mod.o section_parsers.o game_mod.o advent_main.o
	$(FC) $(LDFLAGS_STATIC) -o $@ $^

clean:
	rm *.o *.mod
