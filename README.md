# Colossal Cave Adventure

> *You are standing at the end of a road before a small brick building.*

The original **Colossal Cave Adventure** — the source code for Crowther and
Woods' 350-point version, preserved in its original PDP-10 FORTRAN, plus a
modern Fortran 2023 port.

---

## About the Game

Colossal Cave Adventure (also known simply as *Adventure* or *ADVENT*) was
written by Will Crowther around 1975–1976 and later expanded by Don Woods in
1977. It is widely regarded as the progenitor of the text-adventure (interactive
fiction) genre and a foundational piece of computer-game history. Players
explore a vast underground cave system, collecting treasure and solving puzzles
through typed natural-language commands.

Every port of the game — in FORTRAN, C, Python, or any other language — can be
traced back to this original codebase.

For a detailed history of the game see the homepage:
<http://rickadams.org/adventure/>

---

## Repository Contents

| File / Directory | Description |
|---|---|
| `advent.for` | Original PDP-10 FORTRAN source (350-point version) |
| `advent.dat` | Game data file (rooms, objects, vocabulary, messages) |
| `advent.mic` | PDP-10 TOPS-10 `.MIC` build script (historical) |
| `advent.readme` | Original distribution notes (dated 3/18/96) |
| `advent_f2023_full.f90` | Combined Fortran 2023 port: `advent_mod` + `compat_mod` |
| `io_mod.f90` | I/O module for the modern port |
| `data_mod.f90` | Data-structure routines (`VOCAB`, `MOVE`, `CARRY`, …) |
| `section_parsers.f90` | Database-section parsers for the modern port |
| `game_mod.f90` | Core game-loop module |
| `advent_main.f90` | Main program entry point for the Fortran 2023 port |

---

## Historical Note: PDP-10 FORTRAN Source

The file `advent.for` is an exact preservation of the original source as it ran
on the PDP-10 under TOPS-10. It relies on PDP-10-specific string packing (five
ASCII characters per 36-bit word), so **it cannot be compiled on modern
hardware without significant porting effort**. It is included here as a
historical artifact.

The `.MIC` script (`advent.mic`) is the original build recipe for the PDP-10
TOPS-10 monitor command language:

```
.pa 1:=dsk:
R fortra
*advent=advent
.R link
advent
/go
start
x
del advent.rel
```

---

## Modern Fortran 2023 Port

This repository also contains a clean Fortran 2023 port that compiles on any
standard-conforming Fortran compiler (e.g., GFortran 13+ or Intel `ifx`).

### Build order

Compile the source files in the following order (as documented in
`advent_main.f90`):

```sh
gfortran -c advent_f2023_full.f90   # compiles advent_mod + compat_mod
gfortran -c io_mod.f90
gfortran -c data_mod.f90
gfortran -c section_parsers.f90
gfortran -c game_mod.f90
gfortran -c advent_main.f90
gfortran -o advent advent_f2023_full.o io_mod.o data_mod.o \
          section_parsers.o game_mod.o advent_main.o
```

### Run

```sh
./advent
```

The game reads its world from `advent.dat`, which must be present in the
working directory.

---

## Provenance

The original source was provided courtesy of **Alan H. Martin**
(`AMartin@TLE.ENet.DEC.Com`) from a rescued copy of the LINK-10 regression
test system.

The modern Fortran 2023 port in this repository is a faithful translation of
the original subroutines, preserving the exact game logic.

---

## Links

- **Rick Adams' Adventure page** (history, maps, walkthroughs):
  <http://rickadams.org/adventure/>
- **Wikipedia – Colossal Cave Adventure**:
  <https://en.wikipedia.org/wiki/Colossal_Cave_Adventure>
- **Interactive Fiction Archive**:
  <https://www.ifarchive.org/>

---

*"Magic word XYZZY."*
