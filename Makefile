FC = gfortran
PROGRAM := advent
OBJS := compat_mod.o advent_f2023_full.o io_mod.o data_mod.o section_parsers.o game_mod.o advent_main.o

.PHONY: all clean

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(FC) -o $@ $(OBJS)

compat_mod.o: compat_mod.f90
	$(FC) -c $<

advent_f2023_full.o: advent_f2023_full.f90
	$(FC) -c $<

io_mod.o: io_mod.f90 compat_mod.o advent_f2023_full.o
	$(FC) -c $<

data_mod.o: data_mod.f90 advent_f2023_full.o
	$(FC) -c $<

section_parsers.o: section_parsers.f90 compat_mod.o advent_f2023_full.o
	$(FC) -c $<

game_mod.o: game_mod.f90 advent_f2023_full.o data_mod.o io_mod.o
	$(FC) -c $<

advent_main.o: advent_main.f90 advent_f2023_full.o section_parsers.o game_mod.o
	$(FC) -c $<

clean:
	rm -f $(PROGRAM) *.o *.mod
