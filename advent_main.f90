! Main program entry point for the Fortran 2023 ADVENT port.
! Build order (compile these in order):
!   compat_mod.f90          (compat_mod)
!   advent_f2023_full.f90   (advent_mod)
!   io_mod.f90
!   data_mod.f90
!   section_parsers.f90
!   game_mod.f90
!   advent_main.f90
program advent_main
  use advent_mod
  use section_parsers_mod
  use game_mod
  implicit none

  print '(A)', ' Initializing...'

  call read_database()
  call finalize_database()

  call GAME_INIT()
  call GAME_LOOP()

end program advent_main
