FC = gfortran
GCC = gcc
CFLAGS = -g -O2
LDFLAGS_STATIC = -static-libgcc -static-libgfortran -Wl,-Bstatic -lm -lc -lgfortran -lquadmath -Wl,-Bdynamic

# Compile object files
%.o: %.f90
	$(FC) $(CFLAGS) -c $< -o $@

# Link executable using GCC to ensure static linking of Fortran runtime
advent: compat_mod.o advent_f2023_full.o io_mod.o data_mod.o section_parsers.o game_mod.o advent_main.o
	$(GCC) $(LDFLAGS_STATIC) -o $@ $^

clean:
	rm *.o *.mod
